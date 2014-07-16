{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Graph (
    Graph, empty, addNode, hasNode, hasRelationship, deleteNode, addRelationship, getOrphansFrom,
    getOrphansTo, cleanOrphanRelationships, deleteRelationship, getRelationshipNodeFrom,
    getRelationshipNodeTo, setNodeLabels, addNodeLabel, getNodeLabels, nodeFilter,
    relationshipFilter, union, difference, intersection, getNodes, getRelationships,
    setProperties, setProperty
    )where

import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Database.Neo4j.Types

type NodeIndex = M.HashMap NodePath Node
type RelIndex = M.HashMap RelPath Relationship
type NodeSet = HS.HashSet NodePath
type LabelNodeIndex = M.HashMap Label NodeSet
type LabelSet = HS.HashSet Label
type NodeLabelIndex = M.HashMap NodePath LabelSet

data Graph = Graph {nodes :: NodeIndex, labels :: LabelNodeIndex, rels :: RelIndex,
                        nodeLabels :: NodeLabelIndex} deriving (Eq, Show)

-- | Create an empty graph
empty :: Graph
empty = Graph {nodes = M.empty, labels = M.empty, rels = M.empty, nodeLabels = M.empty}

-- | Add a node to the graph
addNode :: Node -> Graph -> Graph
addNode n g = g {nodes = M.insert (getNodePath n) n (nodes g)}

-- | Set the properties of a node or relationship in the graph, if not present it won't do anything
setProperties :: EntityIdentifier a => a -> Properties -> Graph -> Graph
setProperties ei props g = fromMaybe g $ entity >>= return . newEntity >>= return . addEntity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> M.lookup p (nodes g) >>= return . entityObj
                    (EntityRelPath p) -> M.lookup p (rels g) >>= return . entityObj
          newEntity e = setEntityProperties e props
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Set a property of a node or relationship in the graph, if not present it won't do anything
setProperty :: EntityIdentifier a => a -> T.Text -> PropertyValue -> Graph -> Graph
setProperty ei name value g = fromMaybe g $ entity >>= return . newEntity >>= return . addEntity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> M.lookup p (nodes g) >>= return . entityObj
                    (EntityRelPath p) -> M.lookup p (rels g) >>= return . entityObj
          newEntity e = setEntityProperties e $ M.insert name value (getEntityProperties e)
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Whether a node is present in the graph
hasNode :: NodeIdentifier a => a -> Graph -> Bool
hasNode n g = getNodePath n `M.member` nodes g

-- | Get a list with all the nodes in the graph
getNodes :: Graph -> [Node]
getNodes g = M.elems $ nodes g

-- | Whether a node is present in the graph
hasRelationship :: RelIdentifier a => a -> Graph -> Bool 
hasRelationship r g = getRelPath r `M.member` rels g

-- | Delete a node from the graph
deleteNode :: NodeIdentifier a => a -> Graph -> Graph
deleteNode n g = g {nodes = M.delete nodeLoc (nodes g),
                 labels = cleanLabelNodeIndex $ removeNodeFromLabels (labels g) labelsForNode,
                 nodeLabels = M.delete nodeLoc (nodeLabels g)}
    where labelsForNode = M.lookupDefault HS.empty nodeLoc (nodeLabels g)
          nodeLoc = getNodePath n
          removeNodeFromLabels = HS.foldl' (\acc x -> M.insertWith (\_ -> HS.delete nodeLoc) x HS.empty acc)
          cleanLabelNodeIndex = M.filter (/=HS.empty) 

-- | Add a relationship to the graph
addRelationship :: Relationship -> Graph -> Graph
addRelationship r g = g {rels = M.insert (getRelPath r) r (rels g)}

-- | Get a list with all the relationships in the graph
getRelationships :: Graph -> [Relationship]
getRelationships g = M.elems $ rels g

 -- | Get relationships missing their "from" node
getOrphansFrom :: Graph -> [Relationship]
getOrphansFrom g = M.elems $ M.filter noNode (rels g)
    where noNode r = not $ getNodePath (relFrom r) `M.member` nodes g

-- | Get relationships missing their "to" node
getOrphansTo :: Graph -> [Relationship]
getOrphansTo g = M.elems $ M.filter noNode (rels g)
    where noNode r = not $ getNodePath (relTo r) `M.member` nodes g

-- | Remove all relationships with a missing node
cleanOrphanRelationships :: Graph -> Graph
cleanOrphanRelationships g = foldl (flip deleteRelationship) g (getOrphansFrom g ++ getOrphansTo g)

-- | Delete a relationship from the graph
deleteRelationship :: RelIdentifier a => a -> Graph -> Graph
deleteRelationship r g = g {rels = M.delete (getRelPath r) (rels g)}

-- | Get the "node from" from a relationship
getRelationshipNodeFrom :: Relationship -> Graph -> Maybe Node
getRelationshipNodeFrom r g = M.lookup (getNodePath (relFrom r)) (nodes g) 

-- | Get the "node to" from a relationship
getRelationshipNodeTo :: Relationship -> Graph -> Maybe Node
getRelationshipNodeTo r g = M.lookup (getNodePath (relTo r)) (nodes g)

-- | Set what labels a node has
setNodeLabels :: Node -> [Label] -> Graph -> Graph
setNodeLabels n lbls g = g {nodeLabels = M.insert (getNodePath n) (HS.fromList lbls) (nodeLabels g),
                            labels = insertLabels lbls (labels g)}
    where insertLabels xs acc = foldl (\accum x -> M.insertWith HS.union x defaultNodeSet accum) acc xs
          defaultNodeSet = HS.singleton $ getNodePath n

-- | Add a label to a node
addNodeLabel :: Node -> Label -> Graph -> Graph
addNodeLabel n lbl g = g {nodeLabels = M.insertWith HS.union locationForNode (HS.singleton lbl) nodeLabelIndex,
                            labels = M.insertWith HS.union lbl (HS.singleton locationForNode) labelNodeIndex}
    where locationForNode = getNodePath n
          nodeLabelIndex = nodeLabels g
          labelNodeIndex = labels g

-- | Get the labels of a node
getNodeLabels :: Node -> Graph -> LabelSet
getNodeLabels n g = let loc = getNodePath n in M.lookupDefault HS.empty loc (nodeLabels g)

-- | Filter the nodes of a graph
nodeFilter :: (Node -> Bool) -> Graph -> Graph
nodeFilter f g = foldl (\gacc n -> if f n then deleteNode n gacc else gacc) g (M.elems $ nodes g)

-- | Filter the relationships of a graph
relationshipFilter :: (Relationship -> Bool) -> Graph -> Graph
relationshipFilter f g = foldl (\gacc r -> if f r then deleteRelationship r gacc else gacc) g (M.elems $ rels g)

-- | Add two graphs resulting in a graph with all the nodes, labels and relationships of both
-- | If a node/entity is present in both the second one will be chosen
union :: Graph -> Graph -> Graph
union ga gb = addRels ga (addNodes ga gb)
    where addRels g1 g2 = foldl (flip addRelationship) g1 (M.elems $ rels g2)
          addNodes g1 g2 = foldl (flip addNode) g1 (M.elems $ nodes g2)

-- | Remove the nodes and relationships in the first graph that appear in the second
difference :: Graph -> Graph -> Graph
difference ga gb = relationshipFilter relFilterFunc (nodeFilter nodeFilterFunc ga)
    where relFilterFunc r = not $ hasRelationship r gb
          nodeFilterFunc n = not $ hasNode n gb

-- | Have a graph that only has nodes and relationships that are present in both
intersection :: Graph -> Graph -> Graph
intersection ga gb = relationshipFilter relFilterFunc (nodeFilter nodeFilterFunc ga)
    where relFilterFunc r = hasRelationship r gb
          nodeFilterFunc n = hasNode n gb
