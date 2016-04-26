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
    Connection, Hostname, Port, Credentials, newConnection, withConnection, newAuthConnection, withAuthConnection,
    newSecureConnection, withSecureConnection, newSecureAuthConnection, withSecureAuthConnection,
    -- * Main monadic type to handle sequences of commands to Neo4j
    Neo4j(..),
    -- * Constructing and managing node/relationship properties
    Val(..), PropertyValue(..), newval, (|:), Properties, emptyProperties, getProperties, getProperty, setProperties,
        setProperty, deleteProperties, deleteProperty, 
    -- * Managing nodes
    Node, getNodeProperties, createNode, getNode, deleteNode, nodeId, nodePath, runNodeIdentifier, NodeIdentifier(..),
         NodePath(..),
    -- * Managing relationships
    Relationship, Direction(..), RelationshipType, createRelationship, getRelationship, deleteRelationship,
        getRelationships, relId, relPath, allRelationshipTypes, getRelProperties, getRelType, runRelIdentifier,
        getRelationshipFrom, getRelationshipTo, RelIdentifier(..), RelPath(..),
    -- * Managing labels and getting nodes by label
    EntityIdentifier(..), Label, allLabels, getLabels, getNodesByLabelAndProperty, addLabels, changeLabels,
    removeLabel,
    -- * Indexes
    Index(..), createIndex, getIndexes, dropIndex,
    -- * Exceptions
    Neo4jException(..),
    -- * Database version information
    getDatabaseVersion,
    ) where

import Database.Neo4j.Http
import Database.Neo4j.Index
import Database.Neo4j.Label
import Database.Neo4j.Node
import Database.Neo4j.Property
import Database.Neo4j.Relationship
import Database.Neo4j.Types
import Database.Neo4j.Version

-- $use
--
-- In order to start issuing commands to neo4j you must establish a connection, in order to do that you can use
-- the function 'withConnection':
--
-- > withConnection "127.0.0.1" 7474 $ do
-- >    neo <- createNode M.empty
-- >    cypher <- createNode M.empty
-- >    r <- createRelationship "KNOWS" M.empty neo cypher
-- >    ...
--
-- Also most calls have a batch analogue version, with batch mode you can issue several commands to Neo4j at once.
-- In order to issue batches you must use the "Database.Neo4j.Batch" monad, parameters in batch mode can be actual
-- entities already obtained by issuing regular commands or previous batch commands, or even batch futures,
-- that is you can refer to entities created in the same batch, for instance:
--
-- > withConnection "127.0.0.1" 7474 $ do
-- >    g <- B.runBatch $ do
-- >        neo <- B.createNode M.empty
-- >        cypher <- B.createNode M.empty
-- >        B.createRelationship "KNOWS" M.empty neo cypher
-- >    ...
--
-- As you can see this example does the same thing the previous one does but it will be more efficient as it will
-- be translated into only one request to the database.
--
-- Batch commands return a "Database.Neo4j.Graph" object that holds all the information about relationships, nodes
-- and their labels that can be inferred from running a batch command.
--
-- Another example with batches would be for instance remove all the nodes in a "Database.Neo4j.Graph" object
--
-- > withConnection "127.0.0.1" 7474 $ do
-- >    ...
-- >    B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
--
-- For more information about batch commands and graph objects you can refer to their "Database.Neo4j.Batch" and
-- "Database.Neo4j.Graph" modules.
--
-- Properties are hashmaps with key 'Data.Text' and values a custom type called 'PropertyValue'.
-- This custom type tries to use Haskell's type system to match property values to what Neo4j expects, we only allow
-- 'Int64', 'Double', 'Bool' and 'Text' like values and one-level arrays of these.
-- The only restriction we cannot guarantee with these types is that arrays of values must be of the same type.
--
-- In order to create a 'PropertyValue' from a literal or a value of one of the allowed types you can use the 'newval'
-- function or the operator '|:' to create pairs of key values:
--
-- > import qualified Data.HashMap.Lazy as M
-- >
-- > myval = newval False
-- > someProperties = M.fromList ["mytext" |: ("mytext" :: T.Text),
-- >                             "textarrayprop" |: ["a" :: T.Text, "", "adeu"],
-- >                             "int" |: (-12 :: Int64),
-- >                             "intarray" |: [1 :: Int64, 2],
-- >                             "double" |: (-12.23 :: Double),
-- >                             "doublearray" |: [0.1, -12.23 :: Double],
-- >                             "bool" |: False,
-- >                             "aboolproparray" |: [False, True]
-- >                            ]
-- 
-- When unexpected errors occur a 'Neo4jException' will be raised, sometimes with a specific exception value like for
-- instance 'Neo4jNoEntityException', or more generic ones like 'Neo4jHttpException' or 'Neo4jParseException'
-- if the server  returns something totally unexpected. (I'm sure there's still work to do here preparing the code
-- to return more specific exceptions for known scenarios)
-- 
-- About Cypher support for now we allow sending queries with parameters, the result is a collection of column headers
-- and JSON data values, the Graph object has the function addCypher that tries to find
-- nodes and relationships in a cypher query result and insert them in a "Database.Neo4j.Graph" object
--
-- > import qualified Database.Neo4j.Cypher as C
-- >
-- > withConnection host port $ do
-- >    ...
-- >    -- Run a cypher query with parameters
-- >    res <- C.cypher "CREATE (n:Person { name : {name} }) RETURN n" M.fromList [("name", C.newparam ("Pep" :: T.Text))]
-- >
-- >    -- Get all nodes and relationships that this query returned and insert them in a Graph object
-- >    let graph = G.addCypher (C.fromSuccess res) G.empty
-- >
-- >    -- Get the column headers
-- >    let columnHeaders = C.cols $ C.fromSuccess res
-- >
-- >    -- Get the rows of JSON values received
-- >    let values = C.vals $ C.fromSuccess res
