{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Relationship where


import Control.Exception.Lifted (throw, catch)
import Data.Aeson ((.=))

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http


relationshipAPI :: S.ByteString
relationshipAPI = "/db/data/relationship"

-- | Get the ID of a relationship
relId :: Relationship -> S.ByteString
relId n = S.drop (pathLength + 1) (relPath n)
    where pathLength = S.length relationshipAPI

class RelIdentifier a where
    getRelPath :: a -> S.ByteString

instance RelIdentifier Relationship where
    getRelPath = relPath

instance RelIdentifier S.ByteString where
    getRelPath t = relationshipAPI <> "/" <> t

-- | Create a new relationship with a type and a set of properties
createRelationship :: RelationshipType -> Properties -> Node -> Node -> Neo4j Relationship
createRelationship t props nodefrom nodeto = Neo4j $ \conn -> do
            res <- httpCreate4XXExplained conn reqPath reqBody
            case res of
                        Right rel -> return rel
                        Left expl -> wrapExc expl
    where reqPath = nodePath nodefrom <> "/relationships"
          reqBody = J.encode $ J.object ["to" .= runNodeLocation (nodeLocation nodeto), "type" .= t,
                                         "data" .= J.toJSON props]
          wrapExc msg
            | msg == "StartNodeNotFoundException" = throw (Neo4jNoEntityException $ nodePath nodefrom)
            | msg == "EndNodeNotFoundException" = throw (Neo4jNoEntityException $ nodePath nodeto)
            | otherwise = throw (Neo4jHttpException $ T.unpack msg)

-- | Refresh a relationship entity with the contents in the DB
getRelationship :: RelIdentifier a => a -> Neo4j (Maybe Relationship)
getRelationship rel = Neo4j $ \conn -> httpRetrieve conn (getRelPath rel)

-- | Delete a relationship
deleteRelationship :: RelIdentifier a => a -> Neo4j ()
deleteRelationship rel = Neo4j $ \conn -> httpDelete conn (getRelPath rel)

-- | Get all relationships for a node, if the node has disappeared it will raise an exception
getRelationships :: Node -> Direction -> [Label] -> Neo4j [Relationship]
getRelationships n dir lblFilter = Neo4j $ \conn -> 
            httpRetrieveSure conn (nodePath n <> "/relationships/" <> dirStr dir <> filterStr lblFilter) `catch` procEx
    where dirStr Outgoing = "out"
          dirStr Incoming = "in"
          dirStr Any = "all"
          filterStr [] = ""
          filterStr f = "/" <> TE.encodeUtf8 (T.intercalate "%26" f)
          procEx exc@(Neo4jUnexpectedResponseException s)
                  | s == HT.status404 = throw (Neo4jNoEntityException $ nodePath n)
                  | otherwise = throw exc
          procEx exc = throw exc
