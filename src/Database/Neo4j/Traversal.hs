{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Traversal where

import Data.Default

import Control.Applicative
import Control.Exception.Lifted (catch)
import Control.Monad (mzero)
import Network.HTTP.Base (urlEncodeVars)

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http

-- | Different types of uniqueness calculations for a traversal
data Uniqueness = NodeGlobal | RelationshipGlobal | NodePathUnique | RelationshipPath deriving (Eq, Show)

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

-- | Get the body for a traversal request
traversalReqBody :: TraversalDesc -> L.ByteString
traversalReqBody (TraversalDesc ord relfilt uniq depth nodefilt) = J.encode $ J.object [
        "order" .= J.toJSON ord,
        "relationships" .= relfilt,
        "uniqueness" .= uniq,
        depthField depth,
        "return_filter" .= returnField nodefilt
        ]
    where depthField (Left lvl) = "max_depth" .= lvl
          depthField (Right desc) = "prune_evaluator" .= J.object ["language" .= ("javascript" :: T.Text),
                                                                   "body" .= desc]
          returnField (Left filt) = J.object ["language" .= ("builtin" :: T.Text), "body" .= filt]
          returnField (Right desc) = J.object ["language" .= ("javascript" :: T.Text), "body" .= desc]

-- | How to codify traversalDesc values into JSON
instance J.ToJSON TraversalOrder where
    toJSON BreadthFirst = J.String "breadth_first"
    toJSON DepthFirst = J.String "depth_first"

-- | How to codify ReturnFilter values into JSON
instance J.ToJSON ReturnFilter where
    toJSON ReturnAll = J.String "all"
    toJSON ReturnAllButStartNode = J.String "all_but_start_node"

-- | How to codify RelFilter values into JSON
instance J.ToJSON RelFilter where
    toJSON (rType, dir) = J.object ["direction" .= dirToJson dir, "type" .= J.toJSON rType]
        where dirToJson Outgoing = J.String "out"
              dirToJson Incoming = J.String "in"
              dirToJson Any = J.String "all"

-- | How to codify uniqueness values into JSON
instance J.ToJSON (Maybe Uniqueness) where
    toJSON Nothing = J.String "none"
    toJSON (Just NodeGlobal) = J.String "node_global"
    toJSON (Just RelationshipGlobal) = J.String "relationship_global"
    toJSON (Just NodePathUnique) = J.String "node_path"
    toJSON (Just RelationshipPath) = J.String "relationship_path"

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

-- | Direction without possibility of ambiguity
data ConcreteDirection = In | Out deriving (Eq, Show, Ord)

instance J.FromJSON ConcreteDirection where
    parseJSON (J.String "->" ) =  return Out
    parseJSON (J.String "<-" ) =  return In
    parseJSON _ = mzero


-- | Data type to describe a path in a graph, that is a single node or nodes interleaved with relationships
data Path a b = PathLink !a !b ConcreteDirection !(Path a b) | PathEnd !a deriving (Eq, Ord, Show)

-- | Get all the nodes of a path
pathNodes :: Path a b -> [a]
pathNodes (PathEnd n) = [n]
pathNodes (PathLink n _ _ p) = n : pathNodes p

-- | Get all the relationships of a path
pathRels :: Path a b -> [(b, ConcreteDirection)]
pathRels (PathEnd _) = []
pathRels (PathLink _ r d p) = (r, d) : pathRels p

-- | Path that its data are id's
type IdPath = Path NodePath RelPath

-- | How to decodify an IdPath from JSON
instance J.FromJSON IdPath where
    parseJSON (J.Object v) =  do
                  nodes <- (map (getNodePath . NodeUrl)) <$> (v .: "nodes")
                  rels <- (map (getRelPath . RelUrl)) <$> (v .: "relationships")
                  dirs <- v .: "directions"
                  let correctPath = (length nodes) == (length rels - 1) && (length rels == length dirs) 
                  if correctPath
                    then return $ buildPath nodes rels dirs
                    else fail $ "Wrong path nodes: " <> show nodes <> " rels: " <> show rels <> " dirs: " <> show dirs
        where buildPath [n] [] [] = PathEnd n
              buildPath (n:ns) (r:rs) (d:ds) = PathLink n r d (buildPath ns rs ds)
    parseJSON _ = fail "wrong type for path"


-- | Path that its data are full nodes and relationships, not only their id's
type FullPath = Path Node Relationship

-- | How to decodify an IdPath from JSON
instance J.FromJSON FullPath where
    parseJSON (J.Object v) =  do
                  nodes <- v .: "nodes"
                  rels <- v .: "relationships"
                  dirs <- v .: "directions"
                  let correctPath = (length nodes) == (length rels - 1) && (length rels == length dirs) 
                  if correctPath
                    then return $ buildPath nodes rels dirs
                    else fail $ "Wrong path nodes: " <> show nodes <> " rels: " <> show rels <> " dirs: " <> show dirs
        where buildPath [n] [] [] = PathEnd n
              buildPath (n:ns) (r:rs) (d:ds) = PathLink n r d (buildPath ns rs ds)
    parseJSON _ = fail "wrong type for path"

-- | Data type that holds a result for a paged traversal with the URI to get the rest of the pages
data PagedTraversal a = Done | More (T.Text, [a]) deriving (Eq, Ord, Show)

-- | Get the path for a traversal starting from a node
traversalApi :: NodeIdentifier a => a -> S.ByteString
traversalApi n = TE.encodeUtf8 $ (runNodePath $ getNodePath n) <> "/traverse"

-- | Perform a traversal and get the resulting nodes
traverseGetNodes :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [Node]
traverseGetNodes desc start = Neo4j $ \conn -> httpCreate conn (traversalApi start <> "/node") (traversalReqBody desc)

-- | Perform a traversal and get the resulting node and relationship paths
traverseGetPath :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [IdPath]
traverseGetPath desc start = Neo4j $ \conn -> httpCreate conn (traversalApi start <> "/path") (traversalReqBody desc)

-- | Perform a traversal and get the resulting node and relationship entities
traverseGetFullPath :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [FullPath]
traverseGetFullPath desc start = Neo4j $ \conn -> httpCreate conn (traversalApi start <> "/fullpath")
                                                  (traversalReqBody desc)

-- | Perform a traversal and get the resulting relationship entities
traverseGetRels :: NodeIdentifier a => TraversalDesc -> a -> Neo4j [Relationship]
traverseGetRels desc start = Neo4j $ \conn ->
                                       httpCreate conn (traversalApi start <> "/relationship") (traversalReqBody desc)

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
