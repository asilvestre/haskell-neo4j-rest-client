{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mappend)
import Data.Int (Int64)
import Data.Maybe (fromJust)

import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Control.Exception (handle)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test, Node)
--import Test.QuickCheck
--import qualified Test.HUnit as H

import Database.Neo4j

(<>) :: String -> String -> String
(<>) = mappend

-- Get the string of a exception raised by an action
getException :: IO a -> IO (Maybe Neo4jException)
getException action = handle (return . Just) $ do
    _ <- action
    return Nothing

assertException :: Neo4jException -> IO a -> Assertion
assertException exExc action = do
        resExc <- getException action
        checkExc resExc
    where --checkExc :: (Exception e, Show e) => Maybe e -> Assertion
          checkExc Nothing = assertFailure $ "Expected exception " <> show exExc <> " but none raised"
          checkExc (Just e) = assertEqual "" exExc e
    
-- | handy assertEqual inside a Neo4j monad
neo4jEqual :: (Show a, Eq a) => a -> a -> Neo4j ()
neo4jEqual a b = liftIO $ assertEqual "" a b

-- | handy assertBool inside a Neo4j monad
neo4jBool :: Bool -> Neo4j ()
neo4jBool f = liftIO $ assertBool "" f


main :: IO ()
main = $(defaultMainGenerator)

-- TODO: Is it possible to avoid so many type declarations for literals when constructing properties??
-- | Dummy properties
someProperties :: Properties
someProperties = M.fromList ["mytext" |: ("mytext" :: T.Text),
                             "textarrayprop" |: ["a" :: T.Text, "", "adeu"],
                             "int" |: (-12 :: Int64),
                             "intarray" |: [1 :: Int64, 2],
                             "double" |: (-12.23 :: Double),
                             "doublearray" |: [0.1, -12.23 :: Double],
                             "bool" |: False,
                             "aboolproparray" |: [False, True]
                            ]

-- | Default Neo4j port
port :: Port
port = 7474

-- | Default Neo4j host
host :: Hostname
host = "localhost"

-- | Test connecting to a non-existing server
case_NoConnection :: Assertion
case_NoConnection = assertException expException $ withConnection "localhost" 77 $ createNode someProperties
    where expException = Neo4jHttpException "FailedConnectionException2 \"localhost\" 77 False connect: does\
                                             \ not exist (Connection refused)"

-- | Test get and create a node
case_CreateGetDeleteNode :: Assertion
case_CreateGetDeleteNode = withConnection host port $ do
    n <- createNode someProperties
    newN <- getNode n
    neo4jEqual (Just n) newN
    deleteNode n

-- | Test delete and get a node
case_CreateDeleteGetNode :: Assertion
case_CreateDeleteGetNode = withConnection host port $ do
    n <- createNode someProperties
    deleteNode n
    newN <- getNode n
    neo4jEqual Nothing newN

-- | Test double delete
case_DoubleDeleteNode :: Assertion
case_DoubleDeleteNode = withConnection host port $ do
    n <- createNode someProperties
    deleteNode n
    deleteNode n

-- | Test get node by id
case_GetNodeById :: Assertion
case_GetNodeById = withConnection host port $ do
    n <- createNode someProperties
    newN <- getNode (nodeId n)
    neo4jEqual (Just n) newN
    deleteNode n

-- | Test get node with unexisting id
case_GetUnexistingNodeById :: Assertion
case_GetUnexistingNodeById = withConnection host port $ do
    newN <- getNode ("unexistingnode" :: S.ByteString)
    neo4jEqual Nothing newN
        
-- | Test delete node by id
case_DeleteNodeById :: Assertion
case_DeleteNodeById = withConnection host port $ do
    n <- createNode someProperties
    deleteNode (nodeId n)

-- | Test delete unexisting node by id
case_DeleteUnexistingNodeById :: Assertion
case_DeleteUnexistingNodeById = withConnection host port $ deleteNode ("unexistingnode" :: S.ByteString)

-- | Refresh node properties from the DB
case_getNodeProperties :: Assertion
case_getNodeProperties = withConnection host port $ do
    n <- createNode someProperties
    props <- getProperties n
    neo4jEqual someProperties props
    deleteNode n

-- | Get node properties from a deleted node
case_getDeletedNodeProperties :: Assertion
case_getDeletedNodeProperties = do
        n <- withConnection host port $ createNode someProperties
        let expException = Neo4jNoEntityException $ nodePath n
        assertException expException $ withConnection host port $ do
            deleteNode n
            getProperties n

-- | Get a property from a node
case_getNodeProperty :: Assertion
case_getNodeProperty = withConnection host port $ do
    n <- createNode someProperties
    prop <- getProperty n "intarray"
    neo4jEqual (M.lookup "intarray" someProperties) prop
    deleteNode n

-- | Get an unexisting property from a node
case_getNodeUnexistingProperty :: Assertion
case_getNodeUnexistingProperty = withConnection host port $ do
    n <- createNode someProperties
    prop <- getProperty n "noproperty"
    neo4jEqual Nothing prop
    deleteNode n

-- | Get a property from an unexisting node
case_getUnexistingNodeProperty :: Assertion
case_getUnexistingNodeProperty = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ nodePath n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- getProperty n "noproperty"
        return ()

-- | Get change the properties of a node
case_changeNodeProperties :: Assertion
case_changeNodeProperties = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperties n otherProperties
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert "newbool" (newval True) someProperties

-- | Get change the properties of a node that doesn't exist
case_changeUnexistingNodeProperties :: Assertion
case_changeUnexistingNodeProperties = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ nodePath n
    assertException expException $ withConnection host port $ do
        deleteNode n
        setProperties n someProperties

-- | Change a property of a node
case_changeNodeProperty :: Assertion
case_changeNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "int"
          otherVal = newval False

-- | Change an array property of a node to empty
case_changeNodePropertyToEmpty :: Assertion
case_changeNodePropertyToEmpty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "intarray"
          otherVal = newval ([] :: [Int64])

-- | Set an unexisting property of a node
case_changeNodeUnexistingProperty :: Assertion
case_changeNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "mynewbool"
          otherVal = newval False

-- | Set property of an unexisting node
case_changeUnexistingNodeProperty :: Assertion
case_changeUnexistingNodeProperty = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ nodePath n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- setProperty n otherValName otherVal
        return ()
    where otherValName = "mynewbool"
          otherVal = newval False

-- | Delete node properties
case_deleteNodeProperties :: Assertion
case_deleteNodeProperties = withConnection host port $ do
    n <- createNode someProperties
    newN <- deleteProperties n
    neo4jEqual M.empty (nodeProperties newN)
    renewN <- getNode n
    neo4jEqual M.empty (nodeProperties $ fromJust renewN)
    deleteNode newN

-- | Delete unexisting node properties
case_deleteUnexistingNodeProperties :: Assertion
case_deleteUnexistingNodeProperties = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ nodePath n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- deleteProperties n
        return ()

-- | Delete a node property
case_deleteNodeProperty :: Assertion
case_deleteNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.delete valName someProperties
          valName = "int"

-- | Delete an unexisting property from a node
case_deleteNodeUnexistingProperty :: Assertion
case_deleteNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        neo4jEqual otherProperties (nodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (nodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.delete valName someProperties
          valName = "noproperty"

-- | Delete a property from an unexisting node
case_deleteUnexistingNodeProperty :: Assertion
case_deleteUnexistingNodeProperty = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ nodePath n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- deleteProperty n valName
        return ()
    where valName = "noproperty"

someOtherProperties :: Properties
someOtherProperties = M.fromList ["hola" |: ("adeu" :: T.Text), "proparray" |: ["a" :: T.Text, "", "adeu"]]

anotherProperties :: Properties
anotherProperties = M.empty

myRelType :: T.Text
myRelType = "MYREL"

setupRelTests :: Neo4j (Node, Node, Relationship)
setupRelTests = do
    nodeFrom <- createNode anotherProperties
    nodeTo <- createNode someOtherProperties
    r <- createRelationship myRelType someProperties nodeFrom nodeTo
    return (nodeFrom, nodeTo, r)

teardownRelTests :: Node -> Node -> Relationship -> Neo4j()
teardownRelTests f t r = do
    deleteRelationship r
    deleteNode f
    deleteNode t

-- | Delete a node with a relationship
case_deleteNodeWithRelationship :: Assertion
case_deleteNodeWithRelationship = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNonOrphanNodeDeletionException $ nodePath nodeFrom
    assertException expException $ withConnection host port $ deleteNode nodeFrom
    withConnection host port $ teardownRelTests nodeFrom nodeTo r
    
-- | Test get and create a relationship
case_CreateGetDeleteRelationship :: Assertion
case_CreateGetDeleteRelationship = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- getRelationship r
    neo4jEqual (Just r) newN
    teardownRelTests nodeFrom nodeTo r

-- | Create relationship with missing node from
case_CreateRelationshipMissingFrom :: Assertion
case_CreateRelationshipMissingFrom = do
    (nodeFrom, nodeTo) <- withConnection host port $ do
        nodeFrom <- createNode anotherProperties
        nodeTo <- createNode someOtherProperties
        return (nodeFrom, nodeTo)
    let expException = Neo4jNoEntityException $ nodePath nodeFrom
    assertException expException $ withConnection host port $ do
        deleteNode nodeFrom
        _ <- createRelationship myRelType someProperties nodeFrom nodeTo
        return ()
    withConnection host port $ deleteNode nodeTo

-- | Create relationship with missing node to
case_CreateRelationshipMissingTo :: Assertion
case_CreateRelationshipMissingTo = do
    (nodeFrom, nodeTo) <- withConnection host port $ do
        nodeFrom <- createNode anotherProperties
        nodeTo <- createNode someOtherProperties
        return (nodeFrom, nodeTo)
    let expException = Neo4jNoEntityException $ nodePath nodeTo
    assertException expException $ withConnection host port $ do
        deleteNode nodeTo 
        _ <- createRelationship myRelType someProperties nodeFrom nodeTo
        return ()
    withConnection host port $ deleteNode nodeFrom

-- | Test delete and get a relationship
case_CreateDeleteGetRelationship :: Assertion
case_CreateDeleteGetRelationship = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship r
    newN <- getRelationship r
    neo4jEqual Nothing newN
    teardownRelTests nodeFrom nodeTo r

-- | Test double delete
case_DoubleDeleteRelationship :: Assertion
case_DoubleDeleteRelationship = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship r
    deleteRelationship r
    teardownRelTests nodeFrom nodeTo r

-- | Test get relationship by id
case_GetRelationshipById :: Assertion
case_GetRelationshipById = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- getRelationship (relId r)
    neo4jEqual (Just r) newN
    teardownRelTests nodeFrom nodeTo r

-- | Test get relationship with unexisting id
case_GetUnexistingRelationshipById :: Assertion
case_GetUnexistingRelationshipById = withConnection host port $ do
    newN <- getRelationship ("unexistingrelationship" :: S.ByteString)
    neo4jEqual Nothing newN
        
-- | Test delete relationship by id
case_DeleteRelationshipById :: Assertion
case_DeleteRelationshipById = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship (relId r)
    teardownRelTests nodeFrom nodeTo r

-- | Test delete unexisting relationship by id
case_DeleteUnexistingRelationshipById :: Assertion
case_DeleteUnexistingRelationshipById = withConnection host port $ 
        deleteRelationship ("unexistingrelationship" :: S.ByteString)

-- | Refresh relationship properties from the DB
case_getRelationshipProperties :: Assertion
case_getRelationshipProperties = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    props <- getProperties r
    neo4jEqual someProperties props
    teardownRelTests nodeFrom nodeTo r

-- | Get relationship properties from a deleted node
case_getDeletedRelationshipProperties :: Assertion
case_getDeletedRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ relPath r
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        deleteNode nodeFrom
        deleteNode nodeTo
        getProperties r

-- | Get a property from a relationship
case_getRelationshipProperty :: Assertion
case_getRelationshipProperty = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    prop <- getProperty r "intarray"
    neo4jEqual (M.lookup "intarray" someProperties) prop
    teardownRelTests nodeFrom nodeTo r

-- | Get an unexisting property from a relationship
case_getRelationshipUnexistingProperty :: Assertion
case_getRelationshipUnexistingProperty = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    prop <- getProperty r "noproperty"
    neo4jEqual Nothing prop
    teardownRelTests nodeFrom nodeTo r

-- | Get a property from an unexisting relationship
case_getUnexistingRelationshipProperty :: Assertion
case_getUnexistingRelationshipProperty = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ relPath r
    assertException expException $ withConnection host port $ do
        teardownRelTests nodeFrom nodeTo r
        _ <- getProperty r "noproperty"
        return ()

-- | Get change the properties of a relationship
case_changeRelationshipProperties :: Assertion
case_changeRelationshipProperties = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperties r otherProperties
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert "newbool" (newval True) someProperties

-- | Get change the properties of a relationship that doesn't exist
case_changeUnexistingRelationshipProperties :: Assertion
case_changeUnexistingRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ relPath r
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        deleteNode nodeFrom
        deleteNode nodeTo
        setProperties r someProperties

-- | Change a property of a relationship
case_changeRelationshipProperty :: Assertion
case_changeRelationshipProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperty r otherValName otherVal
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "int"
          otherVal = newval False

-- | Change an array property of a relationship to empty
case_changeRelationshipPropertyToEmpty :: Assertion
case_changeRelationshipPropertyToEmpty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperty r otherValName otherVal
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "intarray"
          otherVal = newval ([] :: [Int64])

-- | Set an unexisting property of a relationship
case_changeRelationshipUnexistingProperty :: Assertion
case_changeRelationshipUnexistingProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperty r otherValName otherVal
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "mynewbool"
          otherVal = newval False

-- | Set property of an unexisting node
case_changeUnexistingRelationshipProperty :: Assertion
case_changeUnexistingRelationshipProperty = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ relPath r
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        _ <- setProperty r otherValName otherVal
        return ()
    withConnection host port $ teardownRelTests nodeFrom nodeTo r
    where otherValName = "mynewbool"
          otherVal = newval False

-- | Delete relationship properties
case_deleteRelationshipProperties :: Assertion
case_deleteRelationshipProperties = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- deleteProperties r
    neo4jEqual M.empty (relProperties newN)
    renewN <- getRelationship r
    neo4jEqual M.empty (relProperties $ fromJust renewN)
    teardownRelTests nodeFrom nodeTo newN

-- | Delete unexisting relationship properties
case_deleteUnexistingRelationshipProperties :: Assertion
case_deleteUnexistingRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ relPath r 
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        _ <- deleteProperties r
        return ()
    withConnection host port $ teardownRelTests nodeFrom nodeTo r

-- | Delete a relationship property
case_deleteRelationshipProperty :: Assertion
case_deleteRelationshipProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- deleteProperty r valName
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.delete valName someProperties
          valName = "int"

-- | Delete an unexisting property from a relationship
case_deleteRelationshipUnexistingProperty :: Assertion
case_deleteRelationshipUnexistingProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- deleteProperty r valName
        neo4jEqual otherProperties (relProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (relProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.delete valName someProperties
          valName = "noproperty"

-- | Delete a property from an unexisting relationship
case_deleteUnexistingRelationshipProperty :: Assertion
case_deleteUnexistingRelationshipProperty = do
        (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
        let expException = Neo4jNoEntityException $ relPath r 
        assertException expException $ withConnection host port $ do
            deleteRelationship r
            _ <- deleteProperty r valName
            return ()
        withConnection host port $ teardownRelTests nodeFrom nodeTo r
    where valName = "noproperty"

-- | Get relationships for a node
case_GetNodeRelationships :: Assertion
case_GetNodeRelationships = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        -- test getting relationships for the origin node
        rsAny <- getRelationships nodeFrom Any []
        neo4jEqual [r] rsAny
        rsOutgoing <- getRelationships nodeFrom Outgoing []
        neo4jEqual [r] rsOutgoing
        rsIncoming <- getRelationships nodeFrom Incoming []
        neo4jEqual [] rsIncoming
        -- test getting relationships for the destination node
        rsAnyTo <- getRelationships nodeTo Any []
        neo4jEqual [r] rsAnyTo
        rsOutgoingTo <- getRelationships nodeTo Outgoing []
        neo4jEqual [] rsOutgoingTo
        rsIncomingTo <- getRelationships nodeTo Incoming []
        neo4jEqual [r] rsIncomingTo
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for an unexisting node
case_GetUnexistingNodeRelationships :: Assertion
case_GetUnexistingNodeRelationships = do
        (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
        let expException = Neo4jNoEntityException $ nodePath nodeFrom
        assertException expException $ withConnection host port $ do
            teardownRelTests nodeFrom nodeTo r
            _ <- getRelationships nodeFrom Any []
            return ()

-- | Get relationships for a node with multiple relationships
case_GetNodeRelationshipsMultiple :: Assertion
case_GetNodeRelationshipsMultiple = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        r2 <- createRelationship myRelType someOtherProperties nodeTo nodeFrom
        -- test getting relationships for the origin node
        rsAny <- getRelationships nodeFrom Any []
        neo4jBool (r `elem` rsAny)
        neo4jBool (r2 `elem` rsAny)
        rsOutgoing <- getRelationships nodeFrom Outgoing []
        neo4jEqual [r] rsOutgoing
        rsIncoming <- getRelationships nodeFrom Incoming []
        neo4jEqual [r2] rsIncoming
        deleteRelationship r2
        teardownRelTests nodeFrom nodeTo r
