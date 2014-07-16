{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch.Relationship where

import Data.Aeson ((.=))
import Data.String (fromString)

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import qualified Database.Neo4j.Graph as G

import Database.Neo4j.Types
import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Types

class RelBatchIdentifier a where
    getRelBatchId :: a -> T.Text

instance RelBatchIdentifier Relationship where
    getRelBatchId = urlMinPath . runRelUrl . relUrl

instance RelBatchIdentifier RelUrl where
    getRelBatchId = urlMinPath . runRelUrl

instance RelBatchIdentifier RelPath where
    getRelBatchId = urlMinPath . runRelPath

instance RelBatchIdentifier (BatchFuture Relationship) where
    getRelBatchId (BatchFuture bId) = "{" <> (fromString . show) bId <> "}"

-- | Create a new relationship with a type and a set of properties
createRelationship :: (NodeBatchIdentifier a, NodeBatchIdentifier b) => RelationshipType -> Properties -> a -> b ->
                         Batch (BatchFuture Relationship)
createRelationship t props nodefrom nodeto = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPost, cmdPath = path, cmdBody = J.toJSON body, cmdParse = parser}
          path = getNodeBatchId nodefrom <> "/relationships"
          body = J.object ["to" .= getNodeBatchId nodeto, "type" .= t, "data" .= J.toJSON props]
          parser r = G.addRelationship (tryParseBody r)

-- | Refresh a relationship entity with the contents in the DB
getRelationship :: RelBatchIdentifier r => r -> Batch (BatchFuture Relationship)
getRelationship rel = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = getRelBatchId rel, cmdBody = "", cmdParse = parser}
          parser jr = G.addRelationship (tryParseBody jr)

-- | Get the "node from" from a relationship from the DB
getRelationshipFrom :: Relationship -> Batch (BatchFuture Node)
getRelationshipFrom rel = getNode $ relFrom rel

-- | Get the "node to" from a relationship from the DB
getRelationshipTo :: Relationship -> Batch (BatchFuture Node)
getRelationshipTo rel = getNode $ relTo rel

-- | Delete a relationship
deleteRelationship :: RelBatchIdentifier r => r -> Batch (BatchFuture ())
deleteRelationship rel = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodDelete, cmdPath = getRelBatchId rel, cmdBody = "", cmdParse = parser}
          parser jr = G.deleteRelationship (RelUrl $ tryParseFrom jr)

-- | Get all relationships for a node
getRelationships :: NodeBatchIdentifier n => n -> Direction -> [RelationshipType] -> Batch (BatchFuture [Relationship])
getRelationships n dir types = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = path, cmdBody = "", cmdParse = parser}
          path = getNodeBatchId n <> "/relationships/" <> dirStr dir <> filterStr types
          parser jr g = foldl (\gacc r ->  G.addRelationship r gacc) g (tryParseBody jr)
          dirStr Outgoing = "out"
          dirStr Incoming = "in"
          dirStr Any = "all"
          filterStr [] = ""
          filterStr f = "/" <> (T.intercalate "%26" f)
