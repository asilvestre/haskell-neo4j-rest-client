{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Module to provide Cypher support using the transactional endpoint.
-- 
--  Example:
--
-- > import qualified Database.Neo4j.Transactional.Cypher as T
-- >
-- > withConnection host port $ do
-- >    ...
-- >    res <- TC.runTransaction $ do
-- >            -- Queries return a result with columns, rows, a list of graphs and stats
-- >            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
-- >                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
-- >                                M.fromList [("age", TC.newparam (78 :: Int64)),
-- >                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
-- >            -- if any of the commands returns an error the transaction is rollbacked and leaves
-- >            result 2 <- T.cypher "not a command" M.empty
-- >            void $ TC.cypher "CREATE (pep: PERSON {age: 55})" M.empty
-- >            -- Transactions are implicitly commited/rollbacked (in case of exception)
-- >            -- but can be explicitly committed and rollbacked
-- >            return (result, result2)
module Database.Neo4j.Transactional.Cypher (
    -- * Types
    Result(..), Stats(..), ParamValue(..), Params, newparam, emptyStats, TransError, Transaction,
    -- * Sending queries
    loneQuery,  runTransaction, cypher, rollback, commit, keepalive, commitWith, rollbackAndLeave,
    -- * Aux functions
    isSuccess, fromResult, fromSuccess
    ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=), (.:))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import qualified Control.Exception as Exc
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as J
import qualified Data.Acquire as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http
import Database.Neo4j.Cypher (ParamValue(..), Params, newparam)
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

-- | Error code and message for a transaction error
type TransError = (T.Text, T.Text)

type TransactionId = S.ByteString

-- | Different transaction states
data TransState = TransInit | TransStarted R.ReleaseKey TransactionId | TransDone

type Transaction a = ExceptT TransError (ReaderT Connection (StateT TransState (R.ResourceT IO))) a

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

-- | Run a transaction and get its final result, has an implicit commit request (or rollback if an exception occurred).
-- This implicit commit/rollback will only be executed if it hasn't before because of an explicit one
runTransaction :: Transaction a -> Neo4j (Either TransError a)
runTransaction t = Neo4j $ \conn ->
                     R.runResourceT (fst <$> runStateT (runReaderT (runExceptT $ catchErrors t) conn) TransInit)

-- | If a query returns an error we have to stop processing the transaction and make sure it's rolled back
catchErrors :: Transaction a -> Transaction a
catchErrors t = catchE t handle
    where handle err@("Rollback", _) = ExceptT $ return (Left err)
          handle err = do
                        rollback
                        ExceptT $ return (Left err)

-- | Run a cypher query in a transaction, if an error occurs the transaction will stop and rollback
cypher :: T.Text -> Params -> Transaction Result
cypher cmd params = do
      conn <- ask
      st <- lift get
      res <- case st of
                -- if this is the first query and the transaction hasn't been created yet, create it
                TransInit -> do 
                        (key, (resp, headers)) <- lift $ lift $ lift $ reqNewTrans conn
                        lift $ put (TransStarted key $ transIdFromHeaders headers)
                        return resp
                -- the transaction is already created
                TransStarted _ transId -> liftIO $ reqTransCreated conn transId
                -- if the transaction has already ended raise an exception
                TransDone -> liftIO $ Exc.throw TransactionEndedExc
      ExceptT $ return $ runResponse res
    where reqNewTrans conn = acquireTrans conn $ httpCreateWithHeaders conn transAPI (queryBody cmd params)
          reqTransCreated conn path = httpCreate conn path (queryBody cmd params)

-- | Rollback a transaction.
--  After this, executing rollback, commit, keepalive, cypher in the transaction will result in an exception
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

-- | Rollback a transaction and stop processing it, set the message that runTransaction will return as error
rollbackAndLeave :: T.Text -> Transaction ()
rollbackAndLeave msg = do
    conn <- ask
    st <- lift get
    case st of
        TransInit -> throwE ("Rollback", msg)
        TransStarted key transId -> do
                        liftIO $ rollbackReq conn transId
                        void $ R.unprotect key
                        lift $ put TransDone
                        throwE ("Rollback", msg)
        TransDone -> liftIO $ Exc.throw TransactionEndedExc

-- | Commit a transaction.
--  After this, executing rollback, commit, keepalive, cypher in the transaction will result in an exception
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

-- | Send a cypher query and commit at the same time, if an error occurs the transaction will be rolled back.
--  After this, executing rollback, commit, keepalive, cypher in the transaction will result in an exception
commitWith :: T.Text -> Params -> Transaction Result
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
      ExceptT $ return $ runResponse res
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
rollbackReq conn trId = void $ httpReq conn HT.methodDelete trId "" (const True)

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
