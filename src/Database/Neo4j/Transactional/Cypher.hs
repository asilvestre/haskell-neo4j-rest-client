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
    loneQuery, isSuccess, fromResult, fromSuccess, runTransaction, cypher, rollback, commit, keepalive, commitWith
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
data TransactionException = TransactionEndedExc deriving (Show, Typeable, Eq)
instance Exc.Exception TransactionException

-- | Different transaction states
data TransState = TransInit | TransStarted R.ReleaseKey TransactionId | TransDone

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
loneQuery =  transactionReq (transAPI <> "/commit")

-- Generate the body for a query
queryBody :: T.Text -> Params -> L.ByteString
queryBody cmd params = J.encode $ J.object ["statements" .= [
                                        J.object ["statement" .= cmd, resultSpec, includeStats,
                                                  "parameters" .= J.toJSON params]]]
    where resultSpec = "resultDataContents" .= ["row", "graph" :: T.Text]
          includeStats = "includeStats" .= True

-- | General function to launch queries inside a transaction
transactionReq :: S.ByteString -> T.Text -> Params -> Neo4j (Either TransError Result)
transactionReq path cmd params = Neo4j $ \conn -> runResponse <$> httpCreate conn path (queryBody cmd params)

-- | Run a transaction and get its final result, has an implicit commit request (or rollback if an exception occurred)
-- | This implicit commit/rollback will only be executed if it hasn't before because of an explicit one
runTransaction :: Transaction a -> Neo4j a
runTransaction t = Neo4j $ \conn -> R.runResourceT (fst <$> runStateT (runReaderT t conn) TransInit)

-- | Run a cypher query in a transaction
cypher :: T.Text -> Params -> Transaction (Either TransError Result)
cypher cmd params = do
      conn <- ask
      st <- lift get
      res <- case st of
                -- if this is the first query and the transaction hasn't been created yet, create it
                TransInit -> do 
                        (key, (resp, headers)) <- lift $ lift $ reqNewTrans conn
                        lift $ put (TransStarted key $ transIdFromHeaders headers)
                        return resp
                -- the transaction is already created
                TransStarted _ transId -> liftIO $ reqTransCreated conn transId
                -- if the transaction has already ended raise an exception
                TransDone -> liftIO $ Exc.throw TransactionEndedExc
      return $ runResponse res
    where reqNewTrans conn = acquireTrans conn $ httpCreateWithHeaders conn transAPI (queryBody cmd params)
          reqTransCreated conn path = httpCreate conn path (queryBody cmd params)

-- | Rollback a transaction
rollback :: Transaction ()
rollback = do
    conn <- ask
    st <- lift get
    case st of
        TransInit -> return ()
        TransStarted key transId -> do
                        liftIO $ rollbackReq conn transId
                        void $ R.unprotect key
                        lift $ put TransDone
        TransDone -> liftIO $ Exc.throw TransactionEndedExc

-- | Commit a transaction
commit :: Transaction ()
commit = do
    conn <- ask
    st <- lift get
    case st of
        TransInit -> return ()
        TransStarted key transId -> do
            liftIO $ commitReq conn transId
            void $ R.unprotect key
            lift $ put TransDone
        TransDone -> liftIO $ Exc.throw TransactionEndedExc

-- | Send a cypher query and commit at the same time
commitWith :: T.Text -> Params -> Transaction (Either TransError Result)
commitWith cmd params = do
      conn <- ask
      st <- lift get
      res <- case st of
                -- if this is the first query and the transaction hasn't been created yet, create it
                TransInit -> liftIO $ req conn (transAPI <> "/commit")
                -- the transaction is already created
                TransStarted key transId -> do
                    void $ R.unprotect key
                    liftIO $ req conn (transId <> "/commit")
                -- if the transaction has already ended raise an exception
                TransDone -> liftIO $ Exc.throw TransactionEndedExc
      lift $ put TransDone
      return $ runResponse res
    where req conn path = httpCreate conn path (queryBody cmd params)

-- | Send a keep alive message to an open transaction
keepalive :: Transaction ()
keepalive = do
    conn <- ask
    st <- lift get
    case st of
        TransInit -> return ()
        TransStarted _ transId -> liftIO $ keepaliveReq conn transId
        TransDone -> liftIO $ Exc.throw TransactionEndedExc

-- | Get the transaction ID from the location header
transIdFromHeaders :: HT.ResponseHeaders -> TransactionId
transIdFromHeaders headers = commitId $ fromMaybe (transAPI <> "-1") (snd <$> find ((==HT.hLocation) . fst) headers)
   where commitId loc = snd $ S.breakSubstring transAPI loc

-- | Start a transaction and register the transaction to be committed/rollbacked
acquireTrans :: Connection -> IO (a, HT.ResponseHeaders) -> R.ResourceT IO (R.ReleaseKey, (a, HT.ResponseHeaders))
acquireTrans conn req = A.allocateAcquire (A.mkAcquireType req freeRes)
    where freeRes (_, headers) A.ReleaseNormal = let transId = transIdFromHeaders headers in commitReq conn transId
          freeRes (_, headers) _ = let transId = transIdFromHeaders headers in rollbackReq conn transId

commitReq :: Connection -> TransactionId -> IO ()
commitReq conn trId = void $ httpReq conn HT.methodPost (trId <> "/commit") "" (const True)

rollbackReq :: Connection -> TransactionId -> IO ()
rollbackReq conn trId = void $ httpReq conn HT.methodPost (trId <> "/rollback") "" (const True)

keepaliveReq :: Connection -> TransactionId -> IO ()
keepaliveReq conn trId = void $ httpReq conn HT.methodPost trId keepaliveBody (const True)
    where keepaliveBody = J.encode $ J.object ["statements" .= ([] :: [Integer])]

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
