{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Node where

import Control.Exception.Lifted (throw, catch)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http
    

-- | Get the ID of a node
nodeId :: Node -> S.ByteString
nodeId n = S.drop (pathLength + 1) (runNodeIdentifier n)
    where pathLength = S.length nodeAPI

-- | Create a new node with a set of properties
createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn -> httpCreate conn nodeAPI (J.encode props)

-- | Refresh a node entity with the contents in the DB
getNode :: NodeIdentifier a => a -> Neo4j (Maybe Node)
getNode n = Neo4j $ \conn -> httpRetrieve conn (runNodeIdentifier n)

-- | Delete a node, if the node has relationships it will raise a Neo4jNonOrphanNodeDeletion
deleteNode :: NodeIdentifier a => a -> Neo4j () 
deleteNode n = Neo4j $ \conn -> httpDelete conn (runNodeIdentifier n) `catch` processConflict
    where processConflict e@(Neo4jUnexpectedResponseException s) 
            | s == HT.status409 = throw (Neo4jNonOrphanNodeDeletionException $ runNodeIdentifier n)
            | otherwise = throw e
          processConflict e = throw e
