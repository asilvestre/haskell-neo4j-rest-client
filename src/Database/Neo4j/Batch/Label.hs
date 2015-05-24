{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch.Label where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Network.HTTP.Base (urlEncodeVars)

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import qualified Database.Neo4j.Graph as G

import Database.Neo4j.Types
import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Types

parseLabelsPath :: J.Value -> T.Text -> NodeUrl
parseLabelsPath j suf = NodeUrl $ let p = tryParseFrom j in fromMaybe p $ T.stripSuffix suf p

-- | Retrieve all labels for a node, if the node doesn't exist already it will raise an exception
-- | Raises Neo4jNoEntityException if the node doesn't exist
getLabels :: NodeBatchIdentifier a => a -> Batch (BatchFuture [Label])
getLabels n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = path, cmdBody = "", cmdParse = parser}
          path = getNodeBatchId n <> "/labels"
          parser jn = G.setNodeLabels (parseLabelsPath jn "/labels") (tryParseBody jn)

-- | Get all nodes using a label and a property
getNodesByLabelAndProperty :: Label -> Maybe (T.Text, PropertyValue) -> Batch (BatchFuture [Node])
getNodesByLabelAndProperty lbl prop = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = path, cmdBody = "", cmdParse = parser}
          path = "/label/" <> lbl <> "/nodes" <> fromString (propUrl prop)
          propUrl Nothing = ""
          propUrl (Just (name, value)) = '?' : urlEncodeVars [(T.unpack name, lbsToStr $ J.encode value)]
          lbsToStr = S.unpack . L.toStrict
          parser jn g = foldl (\gacc n -> G.addNodeLabel n lbl (G.addNode n gacc)) g (tryParseBody jn :: [Node])

-- | Add labels to a node
-- | Raises Neo4jNoEntityException if the node doesn't exist
addLabels :: NodeBatchIdentifier a => [Label] -> a -> Batch (BatchFuture ())
addLabels lbls n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPost, cmdPath = path, cmdBody = J.toJSON lbls, cmdParse = parser}
          path = getNodeBatchId n <> "/labels"
          parser jn g = let from = parseLabelsPath jn "/labels" in foldl (flip (G.addNodeLabel from)) g (lbls :: [Label])

-- | Change node labels
-- | Raises Neo4jNoEntityException if the node doesn't exist
changeLabels :: NodeBatchIdentifier a => [Label] -> a -> Batch (BatchFuture ())
changeLabels lbls n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPut, cmdPath = path, cmdBody = J.toJSON lbls, cmdParse = parser}
          path = getNodeBatchId n <> "/labels"
          parser jn = G.setNodeLabels (parseLabelsPath jn "/labels") lbls

-- | Remove a label for a node
-- | Raises Neo4jNoEntityException if the node doesn't exist
removeLabel :: NodeBatchIdentifier a => Label -> a -> Batch (BatchFuture ())
removeLabel lbl n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodDelete, cmdPath = path, cmdBody = "", cmdParse = parser}
          suffix = "/labels/" <> lbl
          path = getNodeBatchId n <> suffix
          parser jn = G.deleteNodeLabel (parseLabelsPath jn suffix) lbl
