{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Label where

import Network.HTTP.Base (urlEncodeVars)
import Control.Exception.Lifted (catch)

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http


-- | Get all labels in the DB
allLabels :: Neo4j [Label]
allLabels = Neo4j $ \conn -> httpRetrieveSure conn "/db/data/labels"

-- | Retrieve all labels for a node, if the node doesn't exist already it will raise an exception
-- | Raises Neo4jNoEntityException if the node doesn't exist
getLabels :: Node -> Neo4j [Label]
getLabels n = Neo4j $ \conn -> httpRetrieveSure conn (nodePath n <> "/labels") `catch` proc404Exc n

-- | Get all nodes using a label and a property
getNodesByLabelAndProperty :: Label -> Maybe (T.Text, PropertyValue) -> Neo4j [Node]
getNodesByLabelAndProperty lbl prop = Neo4j $ \conn -> 
        httpRetrieveSure conn ("/db/data/label/" <> TE.encodeUtf8 lbl <> "/nodes" <> propUrl prop)
    where propUrl Nothing = ""
          propUrl (Just (name, value)) = S.pack $ '?' : urlEncodeVars [(T.unpack name, lbsToStr $ J.encode value)]
          lbsToStr = S.unpack . L.toStrict

-- | Add labels to a node
-- | Raises Neo4jNoEntityException if the node doesn't exist
addLabels :: [Label] -> Node -> Neo4j ()
addLabels lbls n = Neo4j $ \conn -> (do
        httpCreate_ conn (nodePath n <> "/labels") (J.encode lbls) 
        return ()) `catch` proc404Exc n

-- | Change node labels
-- | Raises Neo4jNoEntityException if the node doesn't exist
changeLabels :: [Label] -> Node -> Neo4j ()
changeLabels lbls n = Neo4j $ \conn -> httpModify conn (nodePath n <> "/labels") (J.encode lbls) `catch` proc404Exc n

-- | Remove a label for a node
-- | Raises Neo4jNoEntityException if the node doesn't exist
removeLabel :: Label -> Node -> Neo4j ()
removeLabel lbl n = Neo4j $ \conn -> (do
        _ <- httpDeleteNo404 conn (nodePath n <> "/labels/" <> TE.encodeUtf8 lbl)
        return ()) `catch` proc404Exc n
