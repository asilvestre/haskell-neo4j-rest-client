{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Relationship where

import Data.Maybe (fromMaybe)

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http


relationshipAPI :: S.ByteString
relationshipAPI = "/db/data/relationship"

-- | Create a new relationship with a type and a set of properties
createRelationship :: RelationshipType -> Properties -> Node -> Node -> Neo4j Relationship
createRelationship t props nodefrom nodeto = Neo4j $ \conn -> httpCreate conn reqPath reqBody
    where reqPath = nodePath nodefrom <> "/relationships"
          reqBody = J.encode $ J.object ["to" .= nodeLocation nodeto, "type" .= t, "data" .= J.toJSON props]

-- | Get relationship by ID
getRelationshipById :: S.ByteString -> Neo4j (Maybe Relationship)
getRelationshipById idRel = Neo4j $ \conn -> httpRetrieve conn (relationshipAPI <> "/" <> idRel)

-- | Refresh a relationship entity with the contents in the DB
getRelationship :: Relationship -> Neo4j (Maybe Relationship)
getRelationship rel = Neo4j $ \conn -> httpRetrieve conn (relPath rel)

-- | Delete a relationship by ID
deleteRelationshipById :: S.ByteString -> Neo4j ()
deleteRelationshipById idRel = Neo4j $ \conn -> do
            _ <- httpDelete conn (relationshipAPI <> "/" <> idRel) False
            return ()

-- | Delete a relationship
deleteRelationship :: Relationship -> Neo4j ()
deleteRelationship rel = Neo4j $ \conn -> do
            _ <- httpDelete conn (relPath rel) False
            return ()

-- | Relationship direction
data Direction = Outgoing | Incoming | Any

-- | Get all relationships for a node, if the node is not present any more we'll just return []
-- TODO This should return an exception if 404
getRelationships :: Node -> Direction -> [Label] -> Neo4j [Relationship]
getRelationships n dir lblFilter = Neo4j $ \conn -> do
            maybeRes <- httpRetrieve conn (nodePath n <> "/relationships/" <> dirStr dir <> filterStr lblFilter)
            return $ fromMaybe [] maybeRes
    where dirStr Outgoing = "out"
          dirStr Incoming = "in"
          dirStr Any = "all"
          filterStr [] = ""
          filterStr f = "/" <> TE.encodeUtf8 (T.intercalate "%26" (map runLabel f))

-- | API path for relationship properties
relPropertiesAPI :: Relationship -> S.ByteString
relPropertiesAPI rel = relPath rel <> "/properties"

-- | Retrieve relationship properties from the DB, if the entity is not present it will return empty properties
relationshipPropertiesFromDB :: Relationship -> Neo4j Properties
relationshipPropertiesFromDB rel = Neo4j $ \conn -> do
            maybeProps <- httpRetrieve conn (relPropertiesAPI rel)
            return $ fromMaybe emptyProperties maybeProps

-- | Set all relationship properties
setRelationshipProperties :: Relationship -> Properties -> Neo4j Relationship
setRelationshipProperties rel props =  Neo4j $ \conn -> do
            httpModify conn (relPropertiesAPI rel) $ J.encode props
            return $ rel {relProperties = props}

-- | Get a relationship property
getRelationshipProperty :: Relationship -> T.Text -> Neo4j (Maybe PropertyValue)
getRelationshipProperty rel prop =  Neo4j $ \conn -> httpRetrieveValue conn (
            relPropertiesAPI rel <> "/" <> TE.encodeUtf8 prop)

-- | Set a relationship property
setRelationshipProperty :: Relationship -> T.Text -> PropertyValue -> Neo4j Relationship
setRelationshipProperty rel name value =  Neo4j $ \conn -> do
            httpModify conn (relPropertiesAPI rel <> "/" <> TE.encodeUtf8 name) $ J.encode value
            return $ rel {relProperties = H.insert name value (relProperties rel)}
