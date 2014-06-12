{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Node where

import qualified Data.Aeson as J
import qualified Data.ByteString as S

import Database.Neo4j.Types
import Database.Neo4j.Http
    

nodeAPI :: S.ByteString
nodeAPI = "/db/data/node"

-- | Create a new node with a set of properties
createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn -> httpCreate conn nodeAPI (J.encode props)

-- | Get a node by ID
getNodeById :: S.ByteString -> Neo4j (Maybe Node)
getNodeById idNode = Neo4j $ \conn -> httpRetrieve conn (nodeAPI <> "/" <> idNode)

-- | Refresh a node entity with the contents in the DB
getNode :: Node -> Neo4j (Maybe Node)
getNode n = Neo4j $ \conn -> httpRetrieve conn (nodePath n)

-- | Delete a node by ID, the deletion will fail if the node is not orphan, if that happens the result will be False
deleteNodeById :: S.ByteString -> Neo4j Bool
deleteNodeById idNode = Neo4j $ \conn -> httpDelete conn (nodeAPI <> "/" <> idNode) True

-- | Delete a node, the deletion will fail if the node is not orphan, if that happens the result will be False
deleteNode :: Node -> Neo4j Bool
deleteNode n = Neo4j $ \conn -> httpDelete conn (nodePath n) True
