{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Traversal where

import Network.HTTP.Base (urlEncodeVars)
import Control.Exception.Lifted (catch)

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http

-- | Different types of uniqueness calculations for a traversal
data Uniqueness = NodeGlobal | RelationshipGlobal | NodePath | RelationshipPath deriving (Eq, Show)

-- | Traversal mode
data TraversalOrder = BreadthFirst | DepthFirst deriving (Eq, Show)

-- | Built-in return filters
data ReturnFilter = ReturnAll | ReturnAllButStartNode deriving (Eq, Show)

type RelFilter = (RelationshipType, Direction)

-- | Type containing all info describing a traversal request
data TraversalDesc = TraversalDesc {
    travOrder :: TraversalOrder,
    travRelFilter :: [RelFilter],
    travUniqueness :: Maybe Uniqueness,
    travDepth :: Either Integer T.Text,
    travNodeFilter :: Either ReturnFilter T.Text} deriving (Eq, Show)

-- | Perform a traversal and get the resulting nodes
traverseGetNodes :: NodeIdentifier a => TraversalOrder -> [RelFilter] -> Maybe Uniqueness ->
                    Either Integer T.Text -> Either ReturnFilter T.Text -> a -> Neo4j [Node]
traverseGetNodes order rfilter mUniq tdepth tfilter start = _

-- | Perform a traversal and get the resulting node and relationship paths
traverseGetPath :: NodeIdentifier a => TraversalOrder -> [RelFilter] -> Maybe Uniqueness ->
                    Either Integer T.Text -> Either ReturnFilter T.Text -> a -> Neo4j ([NodePath], [RelPath])
traverseGetPath order rfilter mUniq tdepth tfilter start = _

-- | Perform a traversal and get the resulting node and relationship entities
traverseGetFullPath :: NodeIdentifier a => TraversalOrder -> [RelFilter] -> Maybe Uniqueness ->
                       Either Integer T.Text -> Either ReturnFilter T.Text -> a -> Neo4j ([Node], [Relationship])
traverseGetFullPath order rfilter mUniq tdepth tfilter start = _

-- | Perform a traversal and get the resulting relationship entities
traverseGetRels :: NodeIdentifier a => TraversalOrder -> Direction -> Maybe Uniqueness ->
                    Either Integer T.Text -> Either ReturnFilter T.Text -> a -> Neo4j [Relationship]
traverseGetRels order dir mUniq tdepth tfilter start = _


-- | Perform a paged traversal and get the resulting nodes
pagedTraverseGetNodes :: NodeIdentifier a => TraversalOrder -> [RelFilter] -> Maybe Uniqueness ->
                         Either Integer T.Text -> Either ReturnFilter T.Text -> a -> Neo4j [Node]
pagedTraverseGetNodes order rfilter mUniq tdepth tfilter start = _
