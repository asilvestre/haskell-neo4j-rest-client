{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (Monoid, mappend)
import Data.Int (Int64)
import Data.Maybe (fromJust)

import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Exception (handle)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base hiding (Test, Node)

import Database.Neo4j
import qualified Database.Neo4j.Graph as G
import qualified Database.Neo4j.Batch as B

(<>) :: Monoid a => a -> a -> a
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
        let expException = Neo4jNoEntityException $ runNodeIdentifier n
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
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- getProperty n "noproperty"
        return ()

-- | Get change the properties of a node
case_changeNodeProperties :: Assertion
case_changeNodeProperties = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperties n otherProperties
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert "newbool" (newval True) someProperties

-- | Get change the properties of a node that doesn't exist
case_changeUnexistingNodeProperties :: Assertion
case_changeUnexistingNodeProperties = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        setProperties n someProperties

-- | Change a property of a node
case_changeNodeProperty :: Assertion
case_changeNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "int"
          otherVal = newval False

-- | Change an array property of a node to empty
case_changeNodePropertyToEmpty :: Assertion
case_changeNodePropertyToEmpty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "intarray"
          otherVal = newval ([] :: [Int64])

-- | Set an unexisting property of a node
case_changeNodeUnexistingProperty :: Assertion
case_changeNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "mynewbool"
          otherVal = newval False

-- | Set property of an unexisting node
case_changeUnexistingNodeProperty :: Assertion
case_changeUnexistingNodeProperty = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
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
    neo4jEqual M.empty (getNodeProperties newN)
    renewN <- getNode n
    neo4jEqual M.empty (getNodeProperties $ fromJust renewN)
    deleteNode newN

-- | Delete unexisting node properties
case_deleteUnexistingNodeProperties :: Assertion
case_deleteUnexistingNodeProperties = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- deleteProperties n
        return ()

-- | Delete a node property
case_deleteNodeProperty :: Assertion
case_deleteNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.delete valName someProperties
          valName = "int"

-- | Delete an unexisting property from a node
case_deleteNodeUnexistingProperty :: Assertion
case_deleteNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        neo4jEqual otherProperties (getNodeProperties newN)
        renewN <- getNode n
        neo4jEqual otherProperties (getNodeProperties $ fromJust renewN)
        deleteNode newN
    where otherProperties = M.delete valName someProperties
          valName = "noproperty"

-- | Delete a property from an unexisting node
case_deleteUnexistingNodeProperty :: Assertion
case_deleteUnexistingNodeProperty = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
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
    let expException = Neo4jNonOrphanNodeDeletionException $ runNodeIdentifier nodeFrom
    assertException expException $ withConnection host port $ deleteNode nodeFrom
    withConnection host port $ teardownRelTests nodeFrom nodeTo r
    
-- | Test get and create a relationship
case_CreateGetDeleteRelationship :: Assertion
case_CreateGetDeleteRelationship = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- getRelationship r
    neo4jEqual (Just r) newN
    teardownRelTests nodeFrom nodeTo r

-- | Test get all relationship properties
case_allRelationshipProperties :: Assertion
case_allRelationshipProperties = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    rs <- allRelationshipTypes
    neo4jBool $ myRelType `elem` rs
    teardownRelTests nodeFrom nodeTo r

-- | Test get the start and end of a relationship
case_relationshipFromTo :: Assertion
case_relationshipFromTo = withConnection host port $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    nfrom <- getRelationshipFrom r
    neo4jEqual nodeFrom nfrom
    nto <- getRelationshipTo r
    neo4jEqual nodeTo nto
    teardownRelTests nodeFrom nodeTo r

-- | Test get the start of relationship that doesn't exist any more
case_nonExistingRelationshipFrom :: Assertion
case_nonExistingRelationshipFrom = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        deleteNode nodeFrom
        n <- getRelationshipFrom r
        neo4jEqual nodeFrom n -- Doing this to force the evaluation of n (the exception is thrown after parsing)
        return ()
    withConnection host port $ teardownRelTests nodeFrom nodeTo r

-- | Test get the end of relationship that doesn't exist any more
case_nonExistingRelationshipTo :: Assertion
case_nonExistingRelationshipTo = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeTo
    assertException expException $ withConnection host port $ do
        deleteRelationship r
        deleteNode nodeTo
        n <- getRelationshipTo r
        neo4jEqual nodeTo n -- Doing this to force the evaluation of n (the exception is thrown after parsing)
        return ()
    withConnection host port $ teardownRelTests nodeFrom nodeTo r

-- | Create relationship with missing node from
case_CreateRelationshipMissingFrom :: Assertion
case_CreateRelationshipMissingFrom = do
    (nodeFrom, nodeTo) <- withConnection host port $ do
        nodeFrom <- createNode anotherProperties
        nodeTo <- createNode someOtherProperties
        return (nodeFrom, nodeTo)
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
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
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeTo
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
    let expException = Neo4jNoEntityException $ runRelIdentifier r
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
    let expException = Neo4jNoEntityException $ runRelIdentifier r
    assertException expException $ withConnection host port $ do
        teardownRelTests nodeFrom nodeTo r
        _ <- getProperty r "noproperty"
        return ()

-- | Get change the properties of a relationship
case_changeRelationshipProperties :: Assertion
case_changeRelationshipProperties = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperties r otherProperties
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert "newbool" (newval True) someProperties

-- | Get change the properties of a relationship that doesn't exist
case_changeUnexistingRelationshipProperties :: Assertion
case_changeUnexistingRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
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
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "int"
          otherVal = newval False

-- | Change an array property of a relationship to empty
case_changeRelationshipPropertyToEmpty :: Assertion
case_changeRelationshipPropertyToEmpty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperty r otherValName otherVal
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "intarray"
          otherVal = newval ([] :: [Int64])

-- | Set an unexisting property of a relationship
case_changeRelationshipUnexistingProperty :: Assertion
case_changeRelationshipUnexistingProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- setProperty r otherValName otherVal
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "mynewbool"
          otherVal = newval False

-- | Set property of an unexisting node
case_changeUnexistingRelationshipProperty :: Assertion
case_changeUnexistingRelationshipProperty = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
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
    neo4jEqual M.empty (getRelProperties newN)
    renewN <- getRelationship r
    neo4jEqual M.empty (getRelProperties $ fromJust renewN)
    teardownRelTests nodeFrom nodeTo newN

-- | Delete unexisting relationship properties
case_deleteUnexistingRelationshipProperties :: Assertion
case_deleteUnexistingRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r 
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
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.delete valName someProperties
          valName = "int"

-- | Delete an unexisting property from a relationship
case_deleteRelationshipUnexistingProperty :: Assertion
case_deleteRelationshipUnexistingProperty = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        newN <- deleteProperty r valName
        neo4jEqual otherProperties (getRelProperties newN)
        renewN <- getRelationship r
        neo4jEqual otherProperties (getRelProperties $ fromJust renewN)
        teardownRelTests nodeFrom nodeTo newN
    where otherProperties = M.delete valName someProperties
          valName = "noproperty"

-- | Delete a property from an unexisting relationship
case_deleteUnexistingRelationshipProperty :: Assertion
case_deleteUnexistingRelationshipProperty = do
        (nodeFrom, nodeTo, r) <- withConnection host port setupRelTests
        let expException = Neo4jNoEntityException $ runRelIdentifier r 
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
        let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
        assertException expException $ withConnection host port $ do
            teardownRelTests nodeFrom nodeTo r
            _ <- getRelationships nodeFrom Any []
            return ()

-- | Get relationships for a node with multiple relationships
case_GetNodeRelationshipsMultiple :: Assertion
case_GetNodeRelationshipsMultiple = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        r2 <- createRelationship myRelType someOtherProperties nodeTo nodeFrom
        rsAny <- getRelationships nodeFrom Any []
        neo4jBool (r `elem` rsAny)
        neo4jBool (r2 `elem` rsAny)
        neo4jEqual 2 (length rsAny)
        rsOutgoing <- getRelationships nodeFrom Outgoing []
        neo4jEqual [r] rsOutgoing
        rsIncoming <- getRelationships nodeFrom Incoming []
        neo4jEqual [r2] rsIncoming
        deleteRelationship r2
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for a node with a type filter
case_GetNodeRelationshipsWithType :: Assertion
case_GetNodeRelationshipsWithType = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        rsAny <- getRelationships nodeFrom Any [myRelType]
        neo4jEqual [r] rsAny
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for a node with an unexisting type filter
case_GetNodeRelationshipsWithUnexistingType :: Assertion
case_GetNodeRelationshipsWithUnexistingType = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        rsAny <- getRelationships nodeFrom Any ["notype"]
        neo4jEqual [] rsAny
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for a node with a type filter with multiple elements
case_GetNodeRelationshipsWithTypes :: Assertion
case_GetNodeRelationshipsWithTypes = withConnection host port $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        -- Create another relationship with another type
        r2 <- createRelationship type2 anotherProperties nodeTo nodeFrom
        rsAny <- getRelationships nodeFrom Any [myRelType, type2, "notype"]
        neo4jBool (r `elem` rsAny)
        neo4jBool (r2 `elem` rsAny)
        neo4jEqual 2 (length rsAny)
        deleteRelationship r2
        teardownRelTests nodeFrom nodeTo r
    where type2 = "reltype2"

-- | Get all labels in the DB (We don't have control of all the labels the DB has)
case_getLabels :: Assertion
case_getLabels = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl] n
        lbls <- allLabels
        neo4jBool $ lbl `elem` lbls
        deleteNode n
    where lbl = "label1"

-- | Get labels for a node it doesn't have any
case_getNodeLabelsWithNone :: Assertion
case_getNodeLabelsWithNone = withConnection host port $ do
    n <- createNode someProperties
    lbls <- getLabels n
    neo4jEqual [] lbls
    deleteNode n

-- | Get labels for an unexisting node
case_getUnexistingNodeLabels :: Assertion
case_getUnexistingNodeLabels = do
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        _ <- getLabels n
        return ()

-- | Add labels to a node and get them
case_getAddAndGetNodeLabels :: Assertion
case_getAddAndGetNodeLabels = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl1, lbl2] n
        addLabels [lbl3] n
        lbls <- getLabels n
        neo4jEqual 3 (length lbls)
        neo4jBool $ all (`elem` lbls) [lbl1, lbl2, lbl3]
        deleteNode n
    where lbl1 = "mylabel1"
          lbl2 = "mylabel2"
          lbl3 = "mylabel3"

-- | Add labels to an unexisting node
case_addUnexistingNodeLabels :: Assertion
case_addUnexistingNodeLabels = do 
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        addLabels ["mylabel"] n

-- | Change node labels
case_changeNodeLabels :: Assertion
case_changeNodeLabels = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl1, lbl2] n
        changeLabels [lbl3] n
        lbls <- getLabels n
        neo4jEqual [lbl3] lbls
        deleteNode n
    where lbl1 = "mylabel1"
          lbl2 = "mylabel2"
          lbl3 = "otherlabel"

-- | Change node labels to empty
case_changeNodeLabelsToEmpty :: Assertion
case_changeNodeLabelsToEmpty = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl1, lbl2] n
        changeLabels [] n
        lbls <- getLabels n
        neo4jEqual [] lbls
        deleteNode n
    where lbl1 = "mylabel1"
          lbl2 = "mylabel2"

-- | Change labels for an unexisting node
case_changeUnexistingNodeLabels :: Assertion
case_changeUnexistingNodeLabels = do 
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        changeLabels ["mylabel"] n

-- | Remove a label from a node
case_removeNodeLabel :: Assertion
case_removeNodeLabel = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl1, lbl2] n
        removeLabel lbl1 n
        lbls <- getLabels n
        neo4jEqual [lbl2] lbls
        deleteNode n
    where lbl1 = "mylabel1"
          lbl2 = "mylabel2"

-- | Remove an unexisting label from a node (nothing should happen)
case_removeNodeUnexistingLabel :: Assertion
case_removeNodeUnexistingLabel = withConnection host port $ do
        n <- createNode someProperties
        addLabels [lbl1] n
        removeLabel "nolabel" n
        lbls <- getLabels n
        neo4jEqual [lbl1] lbls
        deleteNode n
    where lbl1 = "mylabel1"

-- | Remove label for an unexisting node
case_removeUnexistingNodeLabel :: Assertion
case_removeUnexistingNodeLabel = do 
    n <- withConnection host port $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withConnection host port $ do
        deleteNode n
        removeLabel "mylabel" n

-- | Get all nodes with a label
case_allNodesWithLabel :: Assertion
case_allNodesWithLabel = withConnection host port $ do
        n1 <- createNode someProperties
        n2 <- createNode someProperties
        n3 <- createNode someProperties
        addLabels [lbl1, lbl2] n1
        addLabels [lbl2] n2
        addLabels [lbl1] n3
        ns <- getNodesByLabelAndProperty lbl1 Nothing
        neo4jEqual 2 (length ns)
        neo4jBool $ all (`elem` ns) [n1, n3]
        ns2 <- getNodesByLabelAndProperty "nolbl" Nothing
        neo4jEqual [] ns2
        mapM_ deleteNode [n1, n2, n3]
    where lbl1 = "lbl1"
          lbl2 = "lbl2"

-- | Get all nodes with a label and a property
case_allNodesWithLabelAndProperty :: Assertion
case_allNodesWithLabelAndProperty = withConnection host port $ do
        n1 <- createNode someProperties
        n2 <- createNode someProperties
        n3 <- createNode anotherProperties
        addLabels [lbl1, lbl2] n1
        addLabels [lbl2] n2
        addLabels [lbl1] n3
        ns <- getNodesByLabelAndProperty lbl1 $ Just ("doublearray", newval [0.1, -12.23 :: Double])
        neo4jEqual [n1] ns
        ns2 <- getNodesByLabelAndProperty lbl1 $ Just ("doublearray", newval [0.1, -12.22 :: Double])
        neo4jEqual [] ns2
        mapM_ deleteNode [n1, n2, n3]
    where lbl1 = "lbl1"
          lbl2 = "lbl2"

-- | Create, get and destroy index
case_createGetDropIndex :: Assertion
case_createGetDropIndex = withConnection host port $ do
        dropIndex lbl prop1
        dropIndex lbl prop2
        dropIndex lbl2 prop1
        dropIndex lbl2 prop2
        idx1 <- createIndex lbl prop1
        idx2 <- createIndex lbl prop2
        idx3 <- createIndex lbl2 prop1
        idx4 <- createIndex lbl2 prop2
        idxs <- getIndexes lbl
        neo4jBool $ all (`elem` idxs) [idx1, idx2]
        neo4jBool $ all (not . (`elem` idxs)) [idx3, idx4]
        idxs2 <- getIndexes lbl2
        neo4jBool $ all (not . (`elem` idxs2)) [idx1, idx2]
        neo4jBool $ all (`elem` idxs2) [idx3, idx4]
        dropIndex lbl prop1
        dropIndex lbl prop2
        dropIndex lbl2 prop1
        dropIndex lbl2 prop2
        idxs3 <- getIndexes lbl
        idxs4 <- getIndexes lbl2
        neo4jBool $ all (not . (`elem` idxs3)) [idx1, idx2]
        neo4jBool $ all (not . (`elem` idxs4)) [idx3, idx4]
    where lbl = "mylabel11"
          lbl2 = "mylabel2"
          prop1 = "myprop"
          prop2 = "myprop2"

-- | Test batch, create a node and get it again
case_batchCreateGetNode :: Assertion
case_batchCreateGetNode = withConnection host port $ do
                g <- B.runBatch $ do
                        n <- B.createNode someProperties
                        B.getNode n
                neo4jEqual 1 (length $ G.getNodes g)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g)
                deleteNode (head $ G.getNodes g)

-- | Test batch, create two nodes in a batch
case_batchCreate2Nodes :: Assertion
case_batchCreate2Nodes = withConnection host port $ do
                g <- B.runBatch $ do
                        _ <- B.createNode someProperties
                        B.createNode anotherProperties
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g)
                neo4jBool $ anotherProperties `elem` map getNodeProperties (G.getNodes g)
                mapM_ deleteNode (G.getNodes g)

-- | Test batch, create and delete
case_batchCreateDeleteNode :: Assertion
case_batchCreateDeleteNode = withConnection host port $ do
                g <- B.runBatch $ do
                        n <- B.createNode someProperties
                        B.deleteNode n
                neo4jEqual 0 (length $ G.getNodes g)

-- | Test batch, create two nodes and two relationships between them
case_batchCreateRelationships :: Assertion
case_batchCreateRelationships = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 2 (length $ G.getRelationships g)
                neo4jBool $ someOtherProperties `elem` map getRelProperties (G.getRelationships g)
                neo4jBool $ someProperties `elem` map getRelProperties (G.getRelationships g)
                neo4jBool $ "type1" `elem` map getRelType (G.getRelationships g)
                neo4jBool $ "type2" `elem` map getRelType (G.getRelationships g)
                let r1 : r2 : [] = G.getRelationships g
                neo4jEqual (G.getRelationshipNodeFrom r1 g) (G.getRelationshipNodeTo r2 g)
                neo4jEqual (G.getRelationshipNodeFrom r2 g) (G.getRelationshipNodeTo r1 g)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, create and delete a relationship
case_batchCreateDelRelationships :: Assertion
case_batchCreateDelRelationships = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    r <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.deleteRelationship r
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 0 (length $ G.getRelationships g)
                mapM_ deleteRelationship (G.getRelationships g)
                mapM_ deleteNode (G.getNodes g)

-- | Test batch getRelationshipFrom
case_batchRelationshipNodeFrom :: Assertion
case_batchRelationshipNodeFrom = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    B.createRelationship "type1" someOtherProperties n1 n2
                g2 <- B.runBatch $ B.getRelationshipFrom (head $ G.getRelationships g)
                neo4jEqual 1 (length $ G.getNodes g2)
                neo4jEqual 0 (length $ G.getRelationships g2)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g2)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch getRelationshipTo
case_batchRelationshipNodeTo :: Assertion
case_batchRelationshipNodeTo = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    B.createRelationship "type1" someOtherProperties n1 n2
                g2 <- B.runBatch $ B.getRelationshipTo (head $ G.getRelationships g)
                neo4jEqual 1 (length $ G.getNodes g2)
                neo4jEqual 0 (length $ G.getRelationships g2)
                neo4jBool $ anotherProperties `elem` map getNodeProperties (G.getNodes g2)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, get all relationships with filter
case_batchGetRelationships :: Assertion
case_batchGetRelationships = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                g2 <- B.runBatch $ do
                    let n = head $ G.getNodes g
                    B.getRelationships n Any ["type1", "type2"]
                neo4jEqual 2 (length $ G.getRelationships g2)
                neo4jBool $ someOtherProperties `elem` map getRelProperties (G.getRelationships g2)
                neo4jBool $ someProperties `elem` map getRelProperties (G.getRelationships g2)
                neo4jBool $ "type1" `elem` map getRelType (G.getRelationships g2)
                neo4jBool $ "type2" `elem` map getRelType (G.getRelationships g2)
                mapM_ deleteRelationship (G.getRelationships g)
                mapM_ deleteNode (G.getNodes g)

-- | Test batch, set properties
case_batchSetProperties :: Assertion
case_batchSetProperties = withConnection host port $ do
                let newProperties = M.fromList ["croqueta" |: ("2" :: T.Text)]
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    r <- B.createRelationship "type1" someOtherProperties n1 n2
                    _ <- B.setProperties n1 newProperties
                    B.setProperties r newProperties
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 1 (length $ G.getRelationships g)
                neo4jBool $ newProperties `elem` map getRelProperties (G.getRelationships g)
                neo4jBool $ newProperties `elem` map getNodeProperties (G.getNodes g)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()


-- | Test batch, set property
case_batchSetProperty :: Assertion
case_batchSetProperty = withConnection host port $ do
                let key = "hola"
                let val = newval False
                let newSomeProperties = M.insert key val someProperties
                let newSomeOtherProperties = M.insert key val someOtherProperties
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    r <- B.createRelationship "type1" someOtherProperties n1 n2
                    _ <- B.setProperty n1 key val
                    B.setProperty r key val
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 1 (length $ G.getRelationships g)
                neo4jBool $ newSomeProperties `elem` map getNodeProperties (G.getNodes g)
                neo4jBool $ newSomeOtherProperties `elem` map getRelProperties (G.getRelationships g)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, delete properties
case_batchDeleteProperties :: Assertion
case_batchDeleteProperties = withConnection host port $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    r <- B.createRelationship "type1" someOtherProperties n1 n2
                    _ <- B.deleteProperties n1
                    B.deleteProperties r
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 1 (length $ G.getRelationships g)
                neo4jBool $ emptyProperties `elem` map getRelProperties (G.getRelationships g)
                neo4jBool $ emptyProperties `elem` map getNodeProperties (G.getNodes g)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, delete property
case_batchDeleteProperty :: Assertion
case_batchDeleteProperty= withConnection host port $ do
                let key = "mytext"
                let newSomeProperties = M.delete key someProperties
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    r <- B.createRelationship "type1" someProperties n1 n2
                    _ <- B.deleteProperty n1 key
                    B.deleteProperty r key
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jEqual 1 (length $ G.getRelationships g)
                neo4jBool $ newSomeProperties `elem` map getNodeProperties (G.getNodes g)
                neo4jBool $ newSomeProperties `elem` map getRelProperties (G.getRelationships g)
                _ <- B.runBatch $ do
                    mapM_ B.deleteRelationship (G.getRelationships g)
                    mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, delete an unknown property, it should raise an exception
case_batchDeleteUnknownProperty :: Assertion
case_batchDeleteUnknownProperty= do
        n <- withConnection host port $ createNode someProperties
        let excMsg = "Node[" <> TE.decodeUtf8 (nodeId n) <>"] does not have a property \"" <> prop <> "\""
        let expException = Neo4jNoSuchProperty excMsg
        assertException expException $ withConnection host port $ do
                    g <- B.runBatch $ B.deleteProperty n prop
                    neo4jEqual G.empty g -- Doing this to force evaluation
        withConnection host port $ deleteNode n
    where prop = "noprop" :: T.Text

-- | Test batch, set add labels
case_batchAddLabels :: Assertion
case_batchAddLabels = withConnection host port $ do
                let label1 = "label1"
                let label2 = "label2"
                let label3 = "label3"
                let labelset = HS.fromList [label1, label2, label3]
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    _ <- B.createNode anotherProperties
                    _ <- B.addLabels [label1, label2] n1
                    B.addLabels [label3] n1
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jBool $ labelset `elem` map (`G.getNodeLabels` g) (G.getNodes g)
                neo4jBool $ HS.empty `elem` map (`G.getNodeLabels` g) (G.getNodes g)
                _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, change labels
case_batchChangeLabels :: Assertion
case_batchChangeLabels = withConnection host port $ do
                let label1 = "label1"
                let label2 = "label2"
                let label3 = "label3"
                let labelset = HS.fromList [label2]
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    _ <- B.addLabels [label1, label2, label3] n1
                    B.changeLabels [label2] n1
                neo4jBool $ labelset `elem` map (`G.getNodeLabels` g) (G.getNodes g)
                _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, remove label
case_batchRemoveLabel :: Assertion
case_batchRemoveLabel = withConnection host port $ do
                let label1 = "label1"
                let label2 = "label2"
                let label3 = "label3"
                let labelset = HS.fromList [label2, label3]
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    _ <- B.addLabels [label1, label2, label3] n1
                    B.removeLabel label1 n1
                neo4jBool $ labelset `elem` map (`G.getNodeLabels` g) (G.getNodes g)
                _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes g)
                return ()

-- | Test batch, get all nodes with a label
case_batchAllNodesWithLabel :: Assertion
case_batchAllNodesWithLabel = withConnection host port $ do
        gp <- B.runBatch $ do
            n1 <- B.createNode someProperties
            n2 <- B.createNode someProperties
            n3 <- B.createNode someProperties
            _ <- B.addLabels [lbl1, lbl2] n1
            _ <- B.addLabels [lbl2] n2
            B.addLabels [lbl1] n3
        g <- B.runBatch $ B.getNodesByLabelAndProperty lbl1 Nothing
        neo4jEqual 2 (length $ G.getNodes g)
        neo4jBool $ all (`elem` G.getNodes gp) (G.getNodes g)
        gempty <- B.runBatch $ B.getNodesByLabelAndProperty "nolbl" Nothing
        neo4jEqual [] (G.getNodes gempty)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where lbl1 = "la21bl1"
          lbl2 = "la21bl2"

-- | Test batch, get all nodes with a label and a property
case_batchAllNodesWithLabelAndProperty :: Assertion
case_batchAllNodesWithLabelAndProperty = withConnection host port $ do
        gp <- B.runBatch $ do
            n1 <- B.createNode someProperties
            n2 <- B.createNode someProperties
            n3 <- B.createNode anotherProperties
            _ <- B.addLabels [lbl1, lbl2] n1
            _ <- B.addLabels [lbl2] n2
            B.addLabels [lbl1] n3
        g <- B.runBatch $ B.getNodesByLabelAndProperty lbl1 $ Just ("doublearray", newval [0.1, -12.23 :: Double])
        neo4jEqual 1 (length $ G.getNodes g)
        neo4jBool $ all (`elem` G.getNodes gp) (G.getNodes g)
        g2 <- B.runBatch $ B.getNodesByLabelAndProperty lbl1 $ Just ("doublearray", newval [0.1, -12.22 :: Double])
        neo4jEqual [] (G.getNodes g2)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where lbl1 = "lbl1"
          lbl2 = "lbl2"
