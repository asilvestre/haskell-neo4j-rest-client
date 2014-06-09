{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Relationship where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.ByteString as S

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
