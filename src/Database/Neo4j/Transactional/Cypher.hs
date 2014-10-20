{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Module to provide Cypher support.
--  Currently we allow sending queries with parameters, the result is a collection of column headers
--  and JSON data values, the Graph object has the function addCypher that tries to find
--  nodes and relationships in a cypher query result and insert them in a "Database.Neo4j.Graph" object
--
-- > import qualified Database.Neo4j.Cypher as C
-- >
-- > withConnection host port $ do
-- >    ...
-- >    -- Run a cypher query with parameters
-- >    res <- C.cypher "CREATE (n:Person { name : {name} }) RETURN n" M.fromList [("name", C.newparam ("Pep" :: T.Text))]
-- >
-- >    -- Get all nodes and relationships that this query returned and insert them in a Graph object
-- >    let graph = G.addCypher (C.fromSuccess res) G.empty
-- >
-- >    -- Get the column headers
-- >    let columnHeaders = C.cols $ C.fromSuccess res
-- >
-- >    -- Get the rows of JSON values received
-- >    let values = C.vals $ C.fromSuccess res
module Database.Neo4j.Transactional.Cypher (
    -- * Types
    Result(..), Stats(..), ParamValue(..), Params, newparam, emptyStats,
    -- * Sending queries
    loneQuery, isSuccess, fromResult, fromSuccess, runTransaction, newTransaction, cypher, rollback--, newTransactionWithQuery
   -- cypher, fromResult, fromSuccess, isSuccess
    ) where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=), (.:))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)

import qualified Control.Exception as Exc
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as J
import qualified Data.Acquire as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http
import qualified Database.Neo4j.Graph as G


-- | Holds the connection stats
data Stats = Stats {containsUpdates :: Bool,
                    nodesCreated :: Integer,
                    nodesDeleted :: Integer,
                    propsSet :: Integer,
                    relsCreated :: Integer,
                    relsDeleted :: Integer,
                    lblsAdded :: Integer,
                    lblsRemoved :: Integer,
                    idxAdded :: Integer,
                    idxRemoved :: Integer,
                    constAdded :: Integer,
                    constRemoved :: Integer} deriving (Eq, Show)
                
-- | Type for a Cypher response with tuples containing column name and their values
data Result = Result {cols :: [T.Text], vals :: [[J.Value]], graph :: [G.Graph], stats :: Stats} deriving (Show, Eq)

-- | Default stats
emptyStats :: Stats
emptyStats = Stats False 0 0 0 0 0 0 0 0 0 0 0

-- | An empty response
emptyResponse :: Result
emptyResponse = Result [] [] [] emptyStats

newtype CypherNode = CypherNode {runCypherNode :: (Node, [Label])} deriving (Eq, Show)
newtype CypherRel = CypherRel {runCypherRel :: Relationship} deriving (Eq, Show)

readDefault :: Read a => a -> String -> a
readDefault d = fromMaybe d . readMaybe

readIntDefault :: Integer -> String -> Integer
readIntDefault = readDefault


-- | Instance to parse stats from a JSON
instance J.FromJSON Stats where
    parseJSON (J.Object o) = Stats <$> o .: "contains_updates" <*>
                                       o .: "nodes_created" <*>
                                       o .: "nodes_deleted" <*>
                                       o .: "properties_set" <*>
                                       o .: "relationships_created" <*>
                                       o .: "relationship_deleted" <*>
                                       o .: "labels_added" <*>
                                       o .: "labels_removed" <*>
                                       o .: "indexes_added" <*>
                                       o .: "indexes_removed" <*>
                                       o .: "constraints_added" <*>
                                       o .: "constraints_removed"
    parseJSON _ = mzero

-- | Instance for the node type in cypher responses
instance J.FromJSON CypherNode where
    parseJSON (J.Object o) = CypherNode <$> ((,) <$> parseNode <*> (o .: "labels" >>= J.parseJSON))
        where parseNode = do
                idStr <- o .: "id"
                props <- o .: "properties" >>= J.parseJSON
                return $ Node (getNodePath $ readIntDefault 0 idStr) props
    parseJSON _ = mzero

-- | Instance for the rel type in cypher responses
instance J.FromJSON CypherRel where
    parseJSON (J.Object o) = CypherRel <$> (Relationship <$> (relId <$> o .: "id") <*> o .: "type" <*>
           (o .: "properties" >>= J.parseJSON) <*> (nodeId <$> o .: "startNode") <*> (nodeId <$> o .: "endNode") )
        where relId = getRelPath . readIntDefault 0
              nodeId = getNodePath . readIntDefault 0
    parseJSON _ = mzero

-- | Build a graph from a cypher response
buildGraph :: [CypherNode] -> [CypherRel] -> G.Graph
buildGraph ns = foldl addRel (foldl addNode G.empty ns)
    where addNode g cn = let (n, lbls) = runCypherNode cn in G.setNodeLabels n lbls (n `G.addNode` g)
          addRel g cr = let r = runCypherRel cr in r `G.addRelationship` g
              
newtype DataElem = DataElem {runDataElem :: ([J.Value], G.Graph)} deriving (Eq, Show)

instance J.FromJSON DataElem where
    parseJSON (J.Object o) = DataElem <$> ((,) <$> (o .: "row" >>= J.parseJSON) <*> (o .: "graph" >>= parseGraph))
        where parseGraph (J.Object g) = buildGraph <$> (g .: "nodes" >>= J.parseJSON) <*> 
                    (g .: "relationships" >>= J.parseJSON)
              parseGraph _ = mzero
    parseJSON _ = mzero

-- | How to create a response object from a cypher JSON response
instance J.FromJSON Result where
    parseJSON (J.Object o) = Result <$> (o .: "columns" >>= J.parseJSON) <*> (fst <$> pData) <*> (snd <$> pData)
                                         <*> (o .: "stats" >>= J.parseJSON)
        where parseData v = parseDataElem <$> J.parseJSON v
              parseDataElem ds = unzip $ map runDataElem ds
              pData = o .: "data" >>= parseData
    parseJSON _ = mzero
                        
transAPI :: S.ByteString
transAPI = "/db/data/transaction"

-- | Value for a cypher parmeter value, might be a literal, a property map or a list of property maps
data ParamValue = ParamLiteral PropertyValue | ParamProperties Properties | ParamPropertiesArray [Properties]
     deriving (Show, Eq)

newparam :: PropertyValueConstructor a => a -> ParamValue
newparam = ParamLiteral . newval

-- | Instance toJSON for param values so we can serialize them in queries
instance J.ToJSON ParamValue where
    toJSON (ParamLiteral l) = J.toJSON l
    toJSON (ParamProperties p) = J.toJSON p
    toJSON (ParamPropertiesArray ps) = J.toJSON ps

-- | We use hashmaps to represent Cypher parameters
type Params = M.HashMap T.Text ParamValue

type TransError = (T.Text, T.Text)

type TransactionId = S.ByteString

-- | Rollback exception
data TransactionException = TransactionEnded deriving (Show, Typeable, Eq)
instance Exc.Exception TransactionException

-- | Different transaction states
data TransState = TransInit | TransStarted ReleaseKey TransactionId | TransDone deriving (Show, Eq)

type Transaction a = ReaderT Connection (StateT TransState (R.ResourceT IO)) a

newtype Response = Response {runResponse :: Either TransError Result} deriving (Show, Eq)

instance J.FromJSON Response where
    parseJSON (J.Object o) = do
        errs <- o .: "errors" >>= parseErrs
        case errs of
            Just err -> return $ Response (Left err)
            Nothing -> Response . Right <$> (o .: "results" >>= parseResult)
     where parseErrs (J.Array es) =  if V.null es then return Nothing else Just <$> parseErr (V.head es)
           parseErrs _ = mzero
           parseErr (J.Object e) = (,) <$> e .: "code" <*> e .: "message"
           parseErr _ = mzero
           parseResult (J.Array rs) = if V.null rs then return emptyResponse else J.parseJSON $ V.head rs
           parseResult _ = mzero
    parseJSON _ = mzero

-- Transaction in one request
loneQuery :: T.Text -> Params -> Neo4j (Either TransError Result)
loneQuery cmd params =  transactionReq (transAPI <> "/commit") cmd params 

-- Generate the body for a query
queryBody :: T.Text -> Params -> L.ByteString
queryBody cmd params = J.encode $ J.object ["statements" .= [
                                        J.object ["statement" .= cmd, resultSpec, includeStats,
                                                  "parameters" .= J.object["props" .= J.toJSON params]]]]
    where resultSpec = "resultDataContents" .= ["row", "graph" :: T.Text]
          includeStats = "includeStats" .= True

-- | General function to launch queries inside a transaction
transactionReq :: S.ByteString -> T.Text -> Params -> Neo4j (Either TransError Result)
transactionReq path cmd params = Neo4j $ \conn -> runResponse <$> httpCreate conn path (queryBody cmd params)

-- | Run a transaction and get its final result
runTransaction :: Transaction a -> Neo4j a
runTransaction t = Neo4j $ \conn -> (Right <$> R.runResourceT (fst <$> runStateT (runReaderT t conn) TransInit))

-- | Handle exceptions thrown inside a transaction like rollbacks
handleTransactionExc :: forall a. IO (Either TransError a) -> IO (Either TransError a)
handleTransactionExc = Exc.handle handler
    where handler :: TransactionException -> IO (Either TransError a)
          handler TransactionRollback = return $ Left ("Rollback", "")

-- | Build a new transaction (This won't actually perform anything until the first query is issued)
newTransaction :: Transaction ()
newTransaction = return ()

-- | Run a cypher query in a transaction
cypher :: T.Text -> Params -> Transaction (Either TransError Result)
cypher cmd params = do
      conn <- ask
      st <- lift $ get
      res <- case st of
                -- the transaction is already created
                TransStarted _ transId -> liftIO $ reqTransCreated conn (transAPI <> "/" <> transId)
                -- if this is the first query and the transaction hasn't been created yet, create it
                TransInit -> do 
                        (key, (resp, headers)) <- lift $ lift $ reqNewTrans conn
                        lift $ put (TransStarted key $ transIdFromHeaders headers)
                        return $ resp
      return $ runResponse $ res
    where reqNewTrans conn = acquireTrans conn $ httpCreateWithHeaders conn transAPI (queryBody cmd params)
          reqTransCreated conn path = httpCreate conn path (queryBody cmd params)
    
-- | Rollback a transaction
rollback :: Transaction ()
rollback = liftIO $ Exc.throw TransactionRollback

-- | Commit a transaction
commit :: Transaction ()
commit = do
    conn <- ask
    mTransId <- lift $ get
    case mTransId of
        Just transId -> liftIO $ httpReq conn HT.methodPost (transAPI <> "/" <> transId



-- | Start a transaction with the first query to perform
--newTransactionWithQuery :: T.Text -> Params -> Neo4j (Transaction (Either TransError Result))
--newTransactionWithQuery cmd params = Neo4j $ \conn -> do
--            (resp, headers) <- acquireTrans conn (httpCreateWithHeaders conn transAPI (queryBody cmd params))
--            let result = runResponse resp
--            let transId = transIdFromHeaders headers
--            lift $ put (Just transId)
--            return result
    
-- | Get the transaction ID from the location header
transIdFromHeaders :: HT.ResponseHeaders -> TransactionId
transIdFromHeaders headers = commitId $ fromMaybe (transAPI <> "-1") $ (snd <$> find ((==HT.hLocation) . fst) headers)
   where commitId loc = snd $ S.breakSubstring transAPI loc

-- | Start a transaction and register the transaction to be committed/rollbacked
acquireTrans :: Connection -> IO (a, HT.ResponseHeaders) -> R.ResourceT IO (R.ReleaseKey (a, HT.ResponseHeaders))
acquireTrans conn req = A.allocateAcquire (A.mkAcquireType req freeRes)
    where freeRes (_, headers) A.ReleaseNormal = let transId = transIdFromHeaders headers in
             void $ httpReq conn HT.methodPost (transAPI <> "/" <> transId <> "/commit") "" (const True)
          freeRes (_, headers) _ = let transId = transIdFromHeaders headers in
             void $ httpReq conn HT.methodPost (transAPI <> "/" <> transId <> "/rollback") "" (const True)


-- | Run a cypher query
--cypher :: T.Text -> Params -> Neo4j (Either T.Text Response)
--cypher cmd params = Neo4j $ \conn -> httpCreate4XXExplained conn cypherAPI body
--    where body = J.encode $ J.object ["query" .= cmd, "params" .= J.toJSON params]
--
-- | Get the result of the response or a default value
fromResult :: Result -> Either TransError Result -> Result
fromResult def (Left _) = def
fromResult _ (Right resp) = resp

-- | Get the result of the response or a default value
fromSuccess :: Either TransError Result -> Result
fromSuccess (Left _) = error "Cypher.fromSuccess but is Error"
fromSuccess (Right resp) = resp

-- | True if the operation succeeded
isSuccess :: Either TransError Result -> Bool
isSuccess (Left _) = False
isSuccess (Right _) = True
