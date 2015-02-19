{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Traversal where

import Data.Default

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

instance Default TraversalDesc where
    def = TraversalDesc {travOrder = BreadthFirst, travRelFilter = [], travUniqueness = Nothing,
                         travDepth = Left 1, travNodeFilter = Left ReturnAll}

-- | Description of a traversal paging
data TraversalPaging = TraversalPaging {
    pageSize :: Integer,
    pageLeaseSecs :: Integer
    } deriving (Eq, Show)

instance Default TraversalPaging where
    def = TraversalPaging {pageSize = 50, pageLeaseSecs = 60}

-- | Data type that holds a result for a paged traversal with the URI to get the rest of the pages
data PagedTraversal a = Done | More (T.Text, [a]) deriving (Eq, Ord, Show)

-- | Perform a traversal and get the resulting nodes
traverseGetNodes :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [Node]
traverseGetNodes desc start = _

-- | Perform a traversal and get the resulting node and relationship paths
traverseGetPath :: NodeIdentifier a => TraversalDesc -> a -> Neo4j ([NodePath], [RelPath])
traverseGetPath desc start = _

-- | Perform a traversal and get the resulting node and relationship entities
traverseGetFullPath :: NodeIdentifier a => TraversalDesc -> a -> Neo4j ([Node], [Relationship])
traverseGetFullPath desc start = _

-- | Perform a traversal and get the resulting relationship entities
traverseGetRels :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [Relationship]
traverseGetRels desc start = _

-- | Get the values of a paged traversal result
getPagedValues :: PagedTraversal a -> [a]
getPagedValues Done = []
getPagedValues (More (_, r))  = r

-- | Get the next page of values from a traversal result
nextTraversalPage :: PagedTraversal a -> Neo4j (PagedTraversal a)
nextTraversalPage Done = return Done
nextTraversalPage (More (pagingUri, _)) = _

-- | Perform a paged traversal and get the resulting nodes
pagedTraverseGetNodes :: NodeIdentifier a => TraversalDesc -> TraversalPaging -> a -> Neo4j (PagedTraversal Node)
pagedTraverseGetNodes desc paging start = _
