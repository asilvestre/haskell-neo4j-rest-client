{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module:      Database.Neo4j
-- Copyright:   (c) 2014, Antoni Silvestre
-- License:     MIT
-- Maintainer:  Antoni Silvestre <antoni.silvestre@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Library to interact with the Neo4j REST API.
--

module Database.Neo4j (
    -- * How to use this library
    -- $use

    -- * Connection handling objects
    Connection, Hostname, Port, newConnection, withConnection,
    -- * Main monadic type to handle sequences of commands to Neo4j
    Neo4j,
    -- * Constructing and managing node/relationship properties
    Val(..), PropertyValue(..), newval, (|:), Properties, emptyProperties, getProperties, getProperty, setProperties,
        setProperty, deleteProperties, deleteProperty, 
    -- * Managing nodes
    Node, getNodeProperties, createNode, getNode, deleteNode, nodeId, nodePath,
    -- * Managing relationships
    Relationship, Direction(..), RelationshipType, createRelationship, getRelationship, deleteRelationship,
        getRelationships, relId, relPath, allRelationshipTypes, getRelProperties, getRelType,
    -- * Managing labels and getting nodes by label
    Label, allLabels, getLabels, getNodesByLabelAndProperty, addLabels, changeLabels, removeLabel,
    -- * Indexes
    Index(..), createIndex, getIndexes, dropIndex,
    -- * Exceptions
    Neo4jException(..)
    ) where

import Database.Neo4j.Index
import Database.Neo4j.Http
import Database.Neo4j.Label
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Database.Neo4j.Property
import Database.Neo4j.Types
