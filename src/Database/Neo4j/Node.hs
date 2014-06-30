{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Node where

import Control.Exception.Lifted (throw, catch)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http
    

nodeAPI :: S.ByteString
nodeAPI = "/db/data/node"

-- | Get the ID of a node
nodeId :: Node -> S.ByteString
nodeId n = S.drop (pathLength + 1) (nodePath n)
    where pathLength = S.length nodeAPI

class NodeIdentifier a where
    getNodePath :: a -> S.ByteString

instance NodeIdentifier Node where
    getNodePath = nodePath

instance NodeIdentifier S.ByteString where
    getNodePath t = nodeAPI <> "/" <> t

instance NodeIdentifier NodeLocation where
    getNodePath = urlPath . runNodeLocation

-- | Create a new node with a set of properties
createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn -> httpCreate conn nodeAPI (J.encode props)

-- | Refresh a node entity with the contents in the DB
getNode :: NodeIdentifier a => a -> Neo4j (Maybe Node)
getNode n = Neo4j $ \conn -> httpRetrieve conn (getNodePath n)

-- | Delete a node, if the node has relationships it will raise a Neo4jNonOrphanNodeDeletion
deleteNode :: NodeIdentifier a => a -> Neo4j () 
deleteNode n = Neo4j $ \conn -> httpDelete conn (getNodePath n) `catch` processConflict
    where processConflict e@(Neo4jUnexpectedResponseException s) 
            | s == HT.status409 = throw (Neo4jNonOrphanNodeDeletionException $ getNodePath n)
            | otherwise = throw e
          processConflict e = throw e

