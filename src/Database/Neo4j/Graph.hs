{-# LANGUAGE OverloadedStrings  #-}

-- | Module to handle 'Graph' objects. These have information about a group of nodes, relationships,
--  and information about labels per node and nodes per label.
--  Notice a graph can have relationships and at the same time not have some of the nodes of those
--  relationships, see the section called handling orphaned relationships. This is so because commands in a batch
--  might retrieve relationships but might not create or retrieve their respective nodes.
module Database.Neo4j.Graph (
    -- * General objects
    Graph, LabelSet, empty,
    -- * Handling nodes in the graph object
    addNode, hasNode, deleteNode, getNodes, getNode, getNodeFrom, getNodeTo,
    -- * Handling properties in the graph object
    getRelationships, hasRelationship, addRelationship, deleteRelationship, getRelationshipNodeFrom,
    getRelationshipNodeTo, getRelationship,
    -- ** Handling orphaned relationships #orphan#
    getOrphansFrom, getOrphansTo, cleanOrphanRelationships,
    -- * Handling properties
    setProperties, setProperty, deleteProperties, deleteProperty,
    -- * Handling labels
    setNodeLabels, addNodeLabel, getNodeLabels, deleteNodeLabel,
    -- * Handling Cypher results
    addCypher,
    -- * Graph filtering functions
    nodeFilter, relationshipFilter,
    -- * Graph operations
    union, difference, intersection
    )where

import Data.Maybe (fromMaybe)

import Control.Applicative ((<$>))
import Control.Monad (join)
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

import Database.Neo4j.Types
import qualified Database.Neo4j.Cypher as C

type NodeIndex = M.HashMap NodePath Node
type RelIndex = M.HashMap RelPath Relationship
type NodeSet = HS.HashSet NodePath
type LabelNodeIndex = M.HashMap Label NodeSet
type LabelSet = HS.HashSet Label
type NodeLabelIndex = M.HashMap NodePath LabelSet
type RelSet = HS.HashSet RelPath
type NodeRelIndex = M.HashMap NodePath RelSet

data Graph = Graph {nodes :: NodeIndex, labels :: LabelNodeIndex, rels :: RelIndex,
                    nodeLabels :: NodeLabelIndex, nodeFromRels :: NodeRelIndex, nodeToRels :: NodeRelIndex
                    } deriving (Eq, Show)

-- | Create an empty graph
empty :: Graph
empty = Graph {nodes = M.empty, labels = M.empty, rels = M.empty, nodeLabels = M.empty, nodeFromRels = M.empty,
               nodeToRels = M.empty}

-- | Add a node to the graph
addNode :: Node -> Graph -> Graph
addNode n g = g {nodes = M.insert (getNodePath n) n (nodes g)}

-- | Set the properties of a node or relationship in the graph, if not present it won't do anything
setProperties :: EntityIdentifier a => a -> Properties -> Graph -> Graph
setProperties ei props g = fromMaybe g $ (addEntity . newEntity) <$> entity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> entityObj <$> M.lookup p (nodes g)
                    (EntityRelPath p) -> entityObj <$> M.lookup p (rels g)
          newEntity e = setEntityProperties e props
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Set a property of a node or relationship in the graph, if not present it won't do anything
setProperty :: EntityIdentifier a => a -> T.Text -> PropertyValue -> Graph -> Graph
setProperty ei name value g = fromMaybe g $ (addEntity . newEntity) <$> entity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> entityObj <$> M.lookup p (nodes g)
                    (EntityRelPath p) -> entityObj <$> M.lookup p (rels g)
          newEntity e = setEntityProperties e $ M.insert name value (getEntityProperties e)
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Delete all the properties of a node or relationship, if the entity is not present it won't do anything
deleteProperties :: EntityIdentifier a => a -> Graph -> Graph
deleteProperties ei g = fromMaybe g $ (addEntity . newEntity) <$> entity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> entityObj <$> M.lookup p (nodes g)
                    (EntityRelPath p) -> entityObj <$> M.lookup p (rels g)
          newEntity e = setEntityProperties e emptyProperties
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Delete a property of a node or relationship, if the entity is not present it won't do anything
deleteProperty :: EntityIdentifier a => a -> T.Text -> Graph -> Graph
deleteProperty ei key g = fromMaybe g $ (addEntity . newEntity) <$> entity
    where path = getEntityPath ei
          entity = case path of 
                    (EntityNodePath p) -> entityObj <$> M.lookup p (nodes g)
                    (EntityRelPath p) -> entityObj <$> M.lookup p (rels g)
          newEntity e = setEntityProperties e $ M.delete key (getEntityProperties e)
          addEntity e = case e of
                            (EntityNode n) -> g{nodes = M.insert (getNodePath n) n (nodes g)}
                            (EntityRel r) -> g{rels = M.insert (getRelPath r) r (rels g)}

-- | Whether a node is present in the graph
hasNode :: NodeIdentifier a => a -> Graph -> Bool
hasNode n g = getNodePath n `M.member` nodes g

-- | Get a node in the graph
getNode :: NodeIdentifier a => a -> Graph -> Maybe Node
getNode n g = getNodePath n `M.lookup` nodes g

-- | Get outgoing relationships from a node
getNodeFrom :: NodeIdentifier a => a -> Graph -> Maybe [Relationship]
getNodeFrom n g = join $ sequence <$> filter (/=Nothing) <$> map getRel <$> fromrels
    where getRel = flip getRelationship g
          fromrels = HS.toList <$> getNodePath n `M.lookup` nodeFromRels g

-- | Get incoming relationships from a node
getNodeTo :: NodeIdentifier a => a -> Graph -> Maybe [Relationship]
getNodeTo n g = join $ sequence <$> filter (/=Nothing) <$> map getRel <$> torels
    where getRel = flip getRelationship g
          torels = HS.toList <$> getNodePath n `M.lookup` nodeToRels g

-- | Get a list with all the nodes in the graph
getNodes :: Graph -> [Node]
getNodes g = M.elems $ nodes g

-- | Whether a relationship is present in the graph
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
addRelationship r g = g {rels = M.insert (getRelPath r) r (rels g),
                         nodeFromRels = M.insertWith HS.union (pathFrom r) (HS.singleton relpath) (nodeFromRels g),
                         nodeToRels = M.insertWith HS.union (pathTo r) (HS.singleton relpath) (nodeToRels g)}
    where pathFrom = getNodePath . relFrom
          pathTo = getNodePath . relTo
          relpath = getRelPath r

-- | Get a list with all the relationships in the graph
getRelationships :: Graph -> [Relationship]
getRelationships g = M.elems $ rels g

-- | Get a relationship in the graph
getRelationship :: RelIdentifier a => a -> Graph -> Maybe Relationship
getRelationship r g = getRelPath r `M.lookup` rels g

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
deleteRelationship r g = g {rels = M.delete (getRelPath r) (rels g),
                            nodeFromRels = removeNodeRef (nodeFromRels g) pathFrom,
                            nodeToRels = removeNodeRef (nodeToRels g) pathTo}
    where updNodeRel = const $ HS.delete (getRelPath r)
          rel = M.lookup (getRelPath r) (rels g)
          pathFrom = (getNodePath . relFrom) <$> rel
          pathTo = (getNodePath . relTo) <$> rel
          removeNodeRef nodeRel (Just nodepath) = M.insertWith updNodeRel nodepath HS.empty nodeRel
          removeNodeRef nodeRel Nothing = nodeRel


-- | Get the "node from" from a relationship
getRelationshipNodeFrom :: Relationship -> Graph -> Maybe Node
getRelationshipNodeFrom r g = M.lookup (getNodePath (relFrom r)) (nodes g) 

-- | Get the "node to" from a relationship
getRelationshipNodeTo :: Relationship -> Graph -> Maybe Node
getRelationshipNodeTo r g = M.lookup (getNodePath (relTo r)) (nodes g)

-- | Set what labels a node has
setNodeLabels :: NodeIdentifier a => a -> [Label] -> Graph -> Graph
setNodeLabels n lbls g = g {nodeLabels = M.insert (getNodePath n) (HS.fromList lbls) (nodeLabels g),
                            labels = insertLabels lbls (labels g)}
    where insertLabels xs acc = foldl (\accum x -> M.insertWith HS.union x defaultNodeSet accum) acc xs
          defaultNodeSet = HS.singleton $ getNodePath n

-- | Add a label to a node
addNodeLabel :: NodeIdentifier a => a -> Label -> Graph -> Graph
addNodeLabel n lbl g = g {nodeLabels = M.insertWith HS.union locationForNode (HS.singleton lbl) nodeLabelIndex,
                            labels = M.insertWith HS.union lbl (HS.singleton locationForNode) labelNodeIndex}
    where locationForNode = getNodePath n
          nodeLabelIndex = nodeLabels g
          labelNodeIndex = labels g

-- | Get the labels of a node
getNodeLabels :: NodeIdentifier a => a -> Graph -> LabelSet
getNodeLabels n g = let loc = getNodePath n in M.lookupDefault HS.empty loc (nodeLabels g)

-- | Remove a label from a node
deleteNodeLabel :: NodeIdentifier a => a -> Label -> Graph -> Graph
deleteNodeLabel n lbl g = g {nodeLabels = M.insertWith nodeLabelIndexOp locationForNode HS.empty nodeLabelIndex,
                            labels = M.insertWith labelNodeIndexOp lbl HS.empty labelNodeIndex}
    where locationForNode = getNodePath n
          nodeLabelIndex = nodeLabels g
          nodeLabelIndexOp = const $ HS.delete lbl
          labelNodeIndex = labels g
          labelNodeIndexOp = const $ HS.delete locationForNode
          

-- | Filter the nodes of a graph
nodeFilter :: (Node -> Bool) -> Graph -> Graph
nodeFilter f g = foldl (\gacc n -> if f n then gacc else deleteNode n gacc) g (M.elems $ nodes g)

-- | Filter the relationships of a graph
relationshipFilter :: (Relationship -> Bool) -> Graph -> Graph
relationshipFilter f g = foldl (\gacc r -> if f r then gacc else deleteRelationship r gacc) g (M.elems $ rels g)

-- | Add two graphs resulting in a graph with all the nodes, labels and relationships of both
-- | If a node/entity is present in both the second one will be chosen
union :: Graph -> Graph -> Graph
union ga gb = addLabels (addRels (addNodes ga gb) gb) gb
    where addRels g1 g2 = foldl (\gacc r -> addRelationship r gacc) g1 (getRelationships g2)
          addNodes g1 g2 = foldl (flip addNode) g1 (getNodes g2)
          addLabels g1 g2 = foldl (\gacc n -> setNodeLabels n (HS.toList $ getNodeLabels n g2) gacc) g1 (getNodes g2)

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

-- | Feed a cypher result (from the old API) into a graph (looks for nodes and relationships and inserts them)
addCypher :: C.Response -> Graph -> Graph
addCypher (C.Response _ vals) ginit = foldl tryAdd ginit (concat vals)
    where tryAdd :: Graph -> J.Value -> Graph
          -- Try first to parse it as a relationship and then as a node, otherwise leave the graph as it is
          tryAdd g v = fromMaybe g $ L.find parseSuccess (map ($ v) [relParser g, nodeParser g]) >>= fromResult
          relParser g v = flip addRelationship g <$> J.fromJSON v
          nodeParser g v = flip addNode g <$> J.fromJSON v
          parseSuccess (J.Success _) = True
          parseSuccess (J.Error _) = False
          fromResult (J.Error _) = Nothing
          fromResult (J.Success g) = Just g
