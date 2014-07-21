{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch.Property where

import Data.Maybe (fromMaybe)

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import qualified Database.Neo4j.Graph as G

import Database.Neo4j.Types
import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Relationship
import Database.Neo4j.Batch.Types

class BatchEntity a where
    getEntityBatchId :: a -> T.Text

instance BatchEntity Node where
    getEntityBatchId = getNodeBatchId

instance BatchEntity NodeUrl where
    getEntityBatchId = getNodeBatchId

instance BatchEntity NodePath where
    getEntityBatchId = getNodeBatchId

instance BatchEntity (BatchFuture Node) where
    getEntityBatchId = getNodeBatchId

instance BatchEntity Relationship where
    getEntityBatchId = getRelBatchId

instance BatchEntity RelUrl where
    getEntityBatchId = getRelBatchId

instance BatchEntity RelPath where
    getEntityBatchId = getRelBatchId

instance BatchEntity (BatchFuture Relationship) where
    getEntityBatchId = getRelBatchId

parsePropertiesPath :: J.Value -> T.Text -> T.Text
parsePropertiesPath j suf = let p = tryParseFrom j in fromMaybe p $ T.stripSuffix suf p

-- | Set all relationship/node properties
setProperties :: BatchEntity a => a -> Properties -> Batch (BatchFuture ())
setProperties e props = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPut, cmdPath = path, cmdBody = J.toJSON props, cmdParse = parser}
          path = getEntityBatchId e <> "/properties"
          parser f = G.setProperties (parsePropertiesPath f "/properties") props

-- | Set a relationship/node property
setProperty :: BatchEntity a => a -> T.Text -> PropertyValue -> Batch (BatchFuture ())
setProperty e name value = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPut, cmdPath = path, cmdBody = J.toJSON value, cmdParse = parser}
          path = getEntityBatchId e <> pathsuffix
          pathsuffix = "/properties/" <> name
          parser :: J.Value -> G.Graph -> G.Graph
          parser f = G.setProperty (parsePropertiesPath f pathsuffix) name value

-- | Delete all relationship/node properties
deleteProperties :: BatchEntity a => a -> Batch (BatchFuture ())
deleteProperties e = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodDelete, cmdPath = path, cmdBody = "", cmdParse = parser}
          path = getEntityBatchId e <> "/properties"
          parser f = G.deleteProperties (parsePropertiesPath f "/properties")

-- | Delete a relationship/node property
deleteProperty :: BatchEntity a => a -> T.Text -> Batch (BatchFuture ())
deleteProperty e key = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodDelete, cmdPath = path, cmdBody = "", cmdParse = parser}
          path = getEntityBatchId e <> pathsuffix
          pathsuffix = "/properties/" <> key
          parser f = G.deleteProperty (parsePropertiesPath f pathsuffix) key
