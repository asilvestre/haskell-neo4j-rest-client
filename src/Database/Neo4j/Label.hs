{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Label where

import Network.HTTP.Base (urlEncodeVars)

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
getLabels :: Node -> Neo4j [Label]
getLabels n = Neo4j $ \conn -> httpRetrieveSure conn (nodePath n <> "/labels")

-- | Get all nodes using a label and a property
getNodesByLabelAndProperty :: Label -> Maybe (T.Text, PropertyValue) -> Neo4j [Node]
getNodesByLabelAndProperty lbl prop = Neo4j $ \conn -> 
        httpRetrieveSure conn ("/db/data/label/" <> TE.encodeUtf8 lbl <> "/nodes" <> propUrl prop)
    where propUrl Nothing = ""
          propUrl (Just (name, value)) = S.pack $ '?' : urlEncodeVars [(T.unpack name, lbsToStr $ J.encode value)]
          lbsToStr = S.unpack . L.toStrict

-- | Add labels to a node
addLabels :: [Label] -> Node -> Neo4j ()
addLabels lbls n = Neo4j $ \conn -> do
        httpCreate_ conn (nodePath n <> "/labels") (J.encode lbls) 
        return ()

-- | Change node labels
changeLabels :: [Label] -> Node -> Neo4j ()
changeLabels lbls n = Neo4j $ \conn -> httpModify conn (nodePath n <> "/labels") (J.encode lbls) 

-- | Remove a label for a node
removeLabel :: Label -> Node -> Neo4j ()
removeLabel lbl n = Neo4j $ \conn -> do
        _ <- httpDelete conn (nodePath n <> "/labels/" <> TE.encodeUtf8 lbl) False
        return ()

-- | Remove all labels for a node
removeLabels :: Node -> Neo4j ()
removeLabels n = Neo4j $ \conn -> do
        _ <- httpDelete conn (nodePath n <> "/labels") False
        return ()
