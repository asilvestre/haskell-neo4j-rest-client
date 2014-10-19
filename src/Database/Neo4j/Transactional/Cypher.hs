{-# LANGUAGE OverloadedStrings  #-}

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
    loneTransaction, isSuccess, fromResult, fromSuccess
   -- cypher, fromResult, fromSuccess, isSuccess
    ) where

--import Control.Monad.Trans.Except
--import Data.Acquire

import Data.Aeson ((.=), (.:))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Text.Read (readMaybe)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Database.Neo4j.Types
import Database.Neo4j.Http
import qualified Database.Neo4j.Graph as G

import Debug.Trace

--type Transaction a = ExceptT String Neo4j a
newtype Transaction = Transaction Integer deriving (Eq, Ord)

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

newtype Response = Response {runResponse :: Either TransError Result} deriving (Show, Eq)

instance J.FromJSON Response where
    parseJSON (J.Object o) = do
        errs <- o .: "errors" >>= parseErrs
        case errs of
            Just err -> return $ Response (Left err)
            Nothing -> Response . Right <$> (o .: "results" >>= parseResult)
     where parseErrs (J.Array es) =  if trace (show es) V.null es then return Nothing else Just <$> parseErr (V.head es)
           parseErrs _ = mzero
           parseErr (J.Object e) = (,) <$> e .: "code" <*> e .: "message"
           parseErr _ = mzero
           parseResult (J.Array rs) = if trace (show rs) V.null rs then return emptyResponse else J.parseJSON $ V.head rs
           parseResult _ = mzero
    parseJSON _ = mzero

-- Transaction in one request
loneTransaction :: T.Text -> Params -> Neo4j (Either TransError Result)
loneTransaction cmd params = Neo4j $ \conn -> runResponse <$> httpCreate conn (transAPI <> "/commit") body
    where body = J.encode $ J.object ["statements" .= [
                                        J.object ["statement" .= cmd, resultSpec, includeStats,
                                                  "parameters" .= J.object["props" .= J.toJSON params]]]]
          resultSpec = "resultDataContents" .= ["row", "graph" :: T.Text]
          includeStats = "includeStats" .= True

-- Start a transaction with the first parameter
--newTransaction :: T.Text -> Params -> Neo4j Acquire Transaction
--newTransaction = Neo4j $ \conn -> http


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
