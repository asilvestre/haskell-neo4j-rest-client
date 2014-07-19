{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch (
    -- * General
    Batch, runBatch,
    -- * Nodes
    createNode, getNode, deleteNode,
    -- * Relationships
    createRelationship, getRelationship, getRelationshipFrom, getRelationshipTo, deleteRelationship, getRelationships,
    -- * Properties
    setProperties, setProperty, deleteProperties, deleteProperty
    )where

import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Property
import Database.Neo4j.Batch.Relationship
import Database.Neo4j.Batch.Types
