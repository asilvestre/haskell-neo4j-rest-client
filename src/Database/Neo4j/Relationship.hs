{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Relationship where


import Control.Exception.Lifted (throw, catch)
import Data.Aeson ((.=))
import Data.Maybe (fromMaybe)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Node
import Database.Neo4j.Http
import Database.Neo4j.Types


-- | Get the ID of a relationship
relId :: Relationship -> S.ByteString
relId n = S.drop (pathLength + 1) (runRelIdentifier n)
    where pathLength = S.length relationshipAPI

-- | Gets all relationship types in the DB
allRelationshipTypes :: Neo4j [RelationshipType]
allRelationshipTypes = Neo4j $ \conn -> httpRetrieveSure conn "/db/data/relationship/types"

-- | Create a new relationship with a type and a set of properties
createRelationship :: RelationshipType -> Properties -> Node -> Node -> Neo4j Relationship
createRelationship t props nodefrom nodeto = Neo4j $ \conn -> do
            res <- httpCreate4XXExplained conn reqPath reqBody
            case res of
                        Right rel -> return rel
                        Left expl -> wrapExc expl
    where reqPath = runNodeIdentifier nodefrom <> "/relationships"
          reqBody = J.encode $ J.object ["to" .= runNodePath (nodePath nodeto), "type" .= t,
                                         "data" .= J.toJSON props]
          wrapExc msg
            | msg == "StartNodeNotFoundException" = throw (Neo4jNoEntityException $ runNodeIdentifier nodefrom)
            | msg == "EndNodeNotFoundException" = throw (Neo4jNoEntityException $ runNodeIdentifier nodeto)
            | otherwise = throw (Neo4jHttpException $ T.unpack msg)

-- | Refresh a relationship entity with the contents in the DB
getRelationship :: RelIdentifier a => a -> Neo4j (Maybe Relationship)
getRelationship rel = Neo4j $ \conn -> httpRetrieve conn (runRelIdentifier rel)

-- | Get the "node from" from a relationship from the DB
-- | Raises Neo4jNoEntityException if the node (and thus the relationship) does not exist any more
getRelationshipFrom :: Relationship -> Neo4j Node
getRelationshipFrom rel = getNode node >>= processMaybe
    where node = relFrom rel
          processMaybe n = return $ fromMaybe (throw $ Neo4jNoEntityException (runNodeIdentifier node)) n

-- | Get the "node to" from a relationship from the DB
-- | Raises Neo4jNoEntityException if the node (and thus the relationship) does not exist any more
getRelationshipTo :: Relationship -> Neo4j Node
getRelationshipTo rel = getNode node >>= processMaybe
    where node = relTo rel
          processMaybe n = return $ fromMaybe (throw $ Neo4jNoEntityException (runNodeIdentifier node)) n 

-- | Delete a relationship
deleteRelationship :: RelIdentifier a => a -> Neo4j ()
deleteRelationship rel = Neo4j $ \conn -> httpDelete conn (runRelIdentifier rel)

-- | Get all relationships for a node, if the node has disappeared it will raise an exception
getRelationships :: Node -> Direction -> [RelationshipType] -> Neo4j [Relationship]
getRelationships n dir types = Neo4j $ \conn -> 
      httpRetrieveSure conn (runNodeIdentifier n <> "/relationships/" <> dirStr dir <> filterStr types) `catch` procExc
    where dirStr Outgoing = "out"
          dirStr Incoming = "in"
          dirStr Any = "all"
          filterStr [] = ""
          filterStr f = "/" <> TE.encodeUtf8 (T.intercalate "%26" f)
          procExc exc@(Neo4jUnexpectedResponseException s)
                  | s == HT.status404 = throw (Neo4jNoEntityException $ runNodeIdentifier n)
                  | otherwise = throw exc
          procExc exc = throw exc
