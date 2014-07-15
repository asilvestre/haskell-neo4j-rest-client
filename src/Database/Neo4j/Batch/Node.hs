{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch.Node where

import Data.String (fromString)

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import qualified Database.Neo4j.Graph as G

import Database.Neo4j.Batch.Types
import Database.Neo4j.Types

class NodeBatchIdentifier a where
    getNodeBatchId :: a -> T.Text

instance NodeBatchIdentifier Node where
    getNodeBatchId = urlMinPath . runNodeUrl . nodeLocation

instance NodeBatchIdentifier (BatchFuture Node) where
    getNodeBatchId (BatchFuture bId) = "{" <> (fromString . show) bId <> "}"

-- | Batch operation to create a node
createNode :: Properties -> Batch Node
createNode props = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPost, cmdPath = "/node", cmdBody = J.toJSON props, cmdParse = parser}
          parser n = G.addNode (tryParseBody n)

-- | Batch operation to get a node from the DB
getNode :: NodeBatchIdentifier a => a -> Batch Node
getNode n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = getNodeBatchId n, cmdBody = "", cmdParse = parser}
          parser jn = G.addNode (tryParseBody jn)

-- | Batch operation to delete a node
deleteNode :: NodeBatchIdentifier a => a -> Batch ()
deleteNode n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodDelete, cmdPath = getNodeBatchId n, cmdBody = "", cmdParse = parser}
          parser f = G.deleteNode (NodePath (urlPath $ tryParseFrom f))
