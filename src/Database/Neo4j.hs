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
    Connection(..), Hostname, Port, newConnection, withConnection,
    -- * Main monadic type to handle sequences of commands to Neo4j
    Neo4j(..),
    -- * Constructing and managing node/relationship properties
    Val(..), PropertyValue(..), newval, (|:), Properties, emptyProperties, getProperties, getProperty, setProperties,
        setProperty, deleteProperties, deleteProperty, 
    -- * Managing nodes
    Node(..), createNode, getNodeById, getNode, deleteNodeById, deleteNode, nodeId,
    -- * Managing relationships
    Relationship(..), Direction(..), RelationshipType, createRelationship, getRelationshipById, getRelationship,
        deleteRelationshipById, deleteRelationship, getRelationships,
    -- * Managing labels and getting nodes by label
    Label, allLabels, getLabels, getNodesByLabelAndProperty, addLabels, changeLabels, removeLabel, removeLabels,
    -- * Exceptions
    Neo4jException(..),
    test
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Database.Neo4j.Property
import Database.Neo4j.Label

{--
test :: IO Node
test = withConnection "localhost" 7474 $ do
                nodev <- createNode $ M.fromList ["hola" |: ("hhh" :: T.Text)]--[False, True]]
                node <- createNode $ M.fromList [("hola", ValueProperty $ IntVal 1), 
                                                    ("uuu", ArrayProperty [DoubleVal 2.4, DoubleVal 0.99]),
                                                ("adeu", ValueProperty $ TextVal "hol")]
                let nodeId = S.drop (S.length $ nodeAPI <> "/") (TE.encodeUtf8 $ nodeLocation node)
                newNode <- getNodeById nodeId
                otherNewNode <- getNodeById "4"
                liftIO $ print node
                liftIO $ print nodeId 
                liftIO $ print newNode
                liftIO $ print otherNewNode
                hola <- getNode nodev
                case hola of
                    (Just v) -> liftIO $ print v
                    Nothing -> liftIO $ print "HOL"
                return node
--}

test :: IO Node
test = withConnection "localhost" 7474 $ do
                nodev <- createNode $ M.fromList ["hola" |: ("hhh" :: T.Text)]--[False, True]]
                node <- createNode $ M.fromList [("hola", ValueProperty $ IntVal 1), 
                                                    ("uuu", ArrayProperty [DoubleVal 2.4, DoubleVal 0.99]),
                                                ("adeu", ValueProperty $ TextVal "hol")]
                let nodeId = S.drop (S.length $ nodeAPI <> "/") (TE.encodeUtf8 $ nodeLocation node)
                newNode <- getNodeById nodeId
                otherNewNode <- getNodeById "4"
                liftIO $ print node
                liftIO $ print nodeId 
                liftIO $ print newNode
                liftIO $ print otherNewNode
                hola <- getNode nodev
                case hola of
                    (Just v) -> liftIO $ print v
                    Nothing -> liftIO $ print "HOL"
                return node
