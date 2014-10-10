{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch (
    -- * Usage
    -- $use    

    -- * General
    Batch, runBatch, BatchFuture(..), NodeBatchIdentifier, RelBatchIdentifier, BatchEntity,
    -- * Nodes
    createNode, getNode, deleteNode,
    -- * Relationships
    createRelationship, getRelationship, getRelationshipFrom, getRelationshipTo, deleteRelationship, getRelationships,
    -- * Properties
    setProperties, setProperty, deleteProperties, deleteProperty,
    -- * Labels
    getLabels, getNodesByLabelAndProperty, addLabels, changeLabels, removeLabel
    )where

import Database.Neo4j.Batch.Label
import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Property
import Database.Neo4j.Batch.Relationship
import Database.Neo4j.Batch.Types

-- $use
--
-- With batch mode you can issue several commands to Neo4j at once.
-- In order to issue batches you must use the Batch monad, parameters in batch mode can be actual entities already
-- obtained by issuing regular commands or previous batch commands, or even batch futures, that is you can refer
-- to entities created in the same batch, for instance:
--
-- > withConnection "127.0.0.1" 7474 $ do
-- >    g <- B.runBatch $ do
-- >        neo <- B.createNode M.empty
-- >        cypher <- B.createNode M.empty
-- >        B.createRelationship "KNOWS" M.empty neo cypher
-- >    ...
--
-- Batch commands return a "Database.Neo4j.Graph" object that holds all the information about relationships,
-- nodes and their labels that can be inferred from running a batch command.
