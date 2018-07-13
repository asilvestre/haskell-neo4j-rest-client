{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Monoid (Monoid, mappend)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.String (fromString)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Network.HTTP.Types as HT

import Control.Exception (handle)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base hiding (Test, Node)

import Database.Neo4j
import qualified Database.Neo4j.Batch as B
import qualified Database.Neo4j.Cypher as C
import qualified Database.Neo4j.Graph as G
import qualified Database.Neo4j.Transactional.Cypher as TC
import qualified Database.Neo4j.Traversal as T

#if __GLASGOW_HASKELL__ < 841
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

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

assertFException :: (Neo4jException -> Assertion) -> IO a -> Assertion
assertFException f action = do
        resExc <- getException action
        checkExc resExc
    where --checkExc :: (Exception e, Show e) => Maybe e -> Assertion
          checkExc Nothing = assertFailure $ "Expected exception but none raised"
          checkExc (Just e) = f e
    
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

creds :: Credentials
creds = ("neo4j","test")

someOtherProperties :: Properties
someOtherProperties = M.fromList ["hola" |: ("adeu" :: T.Text), "proparray" |: ["a" :: T.Text, "", "adeu"]]

anotherProperties :: Properties
anotherProperties = M.empty

myRelType :: T.Text
myRelType = "MYREL"


-- | Test connecting to a non-existing server
case_NoConnection :: Assertion
case_NoConnection = assertFException f $ withConnection "localhost" 77 $ createNode someProperties
    where f e = assertBool "" $ e == Neo4jHttpException "FailedConnectionException2 \"localhost\" 77 False connect: does not exist (Connection refused)" || e == Neo4jHttpException "HttpExceptionRequest Request {\n  host                 = \"localhost\"\n  port                 = 77\n  secure               = False\n  requestHeaders       = [(\"Accept\",\"application/json; charset=UTF-8\"),(\"Content-Type\",\"application/json\")]\n  path                 = \"/db/data/node\"\n  queryString          = \"\"\n  method               = \"POST\"\n  proxy                = Nothing\n  rawBody              = False\n  redirectCount        = 10\n  responseTimeout      = ResponseTimeoutDefault\n  requestVersion       = HTTP/1.1\n}\n (ConnectionFailure connect: does not exist (Connection refused))"

-- | Test connecting to a server with improper credentials
case_ImproperCredentials :: Assertion
case_ImproperCredentials = do
    mExp <- getException $ withConnection host port $ createNode someProperties
    case mExp of
        Nothing -> return () -- Old Neo4j version or auth disabled, no need to test this
        Just _ -> assertException expException (
            withAuthConnection host port ("fake", "pass") $ createNode someProperties)
    where expException = Neo4jUnexpectedResponseException HT.status401

-- | Test getDatabaseVersion
case_getDatabaseVersion :: Assertion
case_getDatabaseVersion = do 
    withAuthConnection host port creds $ getDatabaseVersion >> return ()

-- | Test get and create a node
case_CreateGetDeleteNode :: Assertion
case_CreateGetDeleteNode = withAuthConnection host port creds $ do
    n <- createNode someProperties
    newN <- getNode n
    neo4jEqual (Just n) newN
    deleteNode n

-- | Test delete and get a node
case_CreateDeleteGetNode :: Assertion
case_CreateDeleteGetNode = withAuthConnection host port creds $ do
    n <- createNode someProperties
    deleteNode n
    newN <- getNode n
    neo4jEqual Nothing newN

-- | Test double delete
case_DoubleDeleteNode :: Assertion
case_DoubleDeleteNode = withAuthConnection host port creds $ do
    n <- createNode someProperties
    deleteNode n
    deleteNode n

-- | Test get node by id
case_GetNodeById :: Assertion
case_GetNodeById = withAuthConnection host port creds $ do
    n <- createNode someProperties
    newN <- getNode (nodeId n)
    neo4jEqual (Just n) newN
    deleteNode n

-- | Test get node with unexisting id
case_GetUnexistingNodeById :: Assertion
case_GetUnexistingNodeById = withAuthConnection host port creds $ do
    newN <- getNode ("unexistingnode" :: S.ByteString)
    neo4jEqual Nothing newN
        
-- | Test delete node by id
case_DeleteNodeById :: Assertion
case_DeleteNodeById = withAuthConnection host port creds $ do
    n <- createNode someProperties
    deleteNode (nodeId n)

-- | Test delete unexisting node by id
case_DeleteUnexistingNodeById :: Assertion
case_DeleteUnexistingNodeById = withAuthConnection host port creds $ deleteNode ("unexistingnode" :: S.ByteString)

-- | Refresh node properties from the DB
case_getNodeProperties :: Assertion
case_getNodeProperties = withAuthConnection host port creds $ do
    n <- createNode someProperties
    props <- getProperties n
    neo4jEqual someProperties props
    deleteNode n

-- | Get node properties from a deleted node
case_getDeletedNodeProperties :: Assertion
case_getDeletedNodeProperties = do
        n <- withAuthConnection host port creds $ createNode someProperties
        let expException = Neo4jNoEntityException $ runNodeIdentifier n
        assertException expException $ withAuthConnection host port creds $ do
            deleteNode n
            getProperties n

-- | Get a property from a node
case_getNodeProperty :: Assertion
case_getNodeProperty = withAuthConnection host port creds $ do
    n <- createNode someProperties
    prop <- getProperty n "intarray"
    neo4jEqual (M.lookup "intarray" someProperties) prop
    deleteNode n

-- | Get an unexisting property from a node
case_getNodeUnexistingProperty :: Assertion
case_getNodeUnexistingProperty = withAuthConnection host port creds $ do
    n <- createNode someProperties
    prop <- getProperty n "noproperty"
    neo4jEqual Nothing prop
    deleteNode n

-- | Get a property from an unexisting node
case_getUnexistingNodeProperty :: Assertion
case_getUnexistingNodeProperty = do
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        _ <- getProperty n "noproperty"
        return ()

-- | Get change the properties of a node
case_changeNodeProperties :: Assertion
case_changeNodeProperties = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        setProperties n someProperties

-- | Change a property of a node
case_changeNodeProperty :: Assertion
case_changeNodeProperty = withAuthConnection host port creds $ do
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
case_changeNodePropertyToEmpty = withAuthConnection host port creds $ do
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
case_changeNodeUnexistingProperty = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        _ <- setProperty n otherValName otherVal
        return ()
    where otherValName = "mynewbool"
          otherVal = newval False

-- | Delete node properties
case_deleteNodeProperties :: Assertion
case_deleteNodeProperties = withAuthConnection host port creds $ do
    n <- createNode someProperties
    newN <- deleteProperties n
    neo4jEqual M.empty (getNodeProperties newN)
    renewN <- getNode n
    neo4jEqual M.empty (getNodeProperties $ fromJust renewN)
    deleteNode newN

-- | Delete unexisting node properties
case_deleteUnexistingNodeProperties :: Assertion
case_deleteUnexistingNodeProperties = do
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        _ <- deleteProperties n
        return ()

-- | Delete a node property
case_deleteNodeProperty :: Assertion
case_deleteNodeProperty = withAuthConnection host port creds $ do
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
case_deleteNodeUnexistingProperty = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        _ <- deleteProperty n valName
        return ()
    where valName = "noproperty"

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
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNonOrphanNodeDeletionException $ runNodeIdentifier nodeFrom
    assertException expException $ withAuthConnection host port creds $ deleteNode nodeFrom
    withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r
    
-- | Test get and create a relationship
case_CreateGetDeleteRelationship :: Assertion
case_CreateGetDeleteRelationship = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- getRelationship r
    neo4jEqual (Just r) newN
    teardownRelTests nodeFrom nodeTo r

-- | Test get all relationship properties
case_allRelationshipProperties :: Assertion
case_allRelationshipProperties = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    rs <- allRelationshipTypes
    neo4jBool $ myRelType `elem` rs
    teardownRelTests nodeFrom nodeTo r

-- | Test get the start and end of a relationship
case_relationshipFromTo :: Assertion
case_relationshipFromTo = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    nfrom <- getRelationshipFrom r
    neo4jEqual nodeFrom nfrom
    nto <- getRelationshipTo r
    neo4jEqual nodeTo nto
    teardownRelTests nodeFrom nodeTo r

-- | Test get the start of relationship that doesn't exist any more
case_nonExistingRelationshipFrom :: Assertion
case_nonExistingRelationshipFrom = do
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        deleteNode nodeFrom
        n <- getRelationshipFrom r
        neo4jEqual nodeFrom n -- Doing this to force the evaluation of n (the exception is thrown after parsing)
        return ()
    withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r

-- | Test get the end of relationship that doesn't exist any more
case_nonExistingRelationshipTo :: Assertion
case_nonExistingRelationshipTo = do
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeTo
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        deleteNode nodeTo
        n <- getRelationshipTo r
        neo4jEqual nodeTo n -- Doing this to force the evaluation of n (the exception is thrown after parsing)
        return ()
    withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r

-- | Create relationship with missing node from
case_CreateRelationshipMissingFrom :: Assertion
case_CreateRelationshipMissingFrom = do
    (nodeFrom, nodeTo) <- withAuthConnection host port creds $ do
        nodeFrom <- createNode anotherProperties
        nodeTo <- createNode someOtherProperties
        return (nodeFrom, nodeTo)
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode nodeFrom
        _ <- createRelationship myRelType someProperties nodeFrom nodeTo
        return ()
    withAuthConnection host port creds $ deleteNode nodeTo

-- | Create relationship with missing node to
case_CreateRelationshipMissingTo :: Assertion
case_CreateRelationshipMissingTo = do
    (nodeFrom, nodeTo) <- withAuthConnection host port creds $ do
        nodeFrom <- createNode anotherProperties
        nodeTo <- createNode someOtherProperties
        return (nodeFrom, nodeTo)
    let expException = Neo4jNoEntityException $ runNodeIdentifier nodeTo
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode nodeTo 
        _ <- createRelationship myRelType someProperties nodeFrom nodeTo
        return ()
    withAuthConnection host port creds $ deleteNode nodeFrom

-- | Test delete and get a relationship
case_CreateDeleteGetRelationship :: Assertion
case_CreateDeleteGetRelationship = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship r
    newN <- getRelationship r
    neo4jEqual Nothing newN
    teardownRelTests nodeFrom nodeTo r

-- | Test double delete
case_DoubleDeleteRelationship :: Assertion
case_DoubleDeleteRelationship = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship r
    deleteRelationship r
    teardownRelTests nodeFrom nodeTo r

-- | Test get relationship by id
case_GetRelationshipById :: Assertion
case_GetRelationshipById = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- getRelationship (relId r)
    neo4jEqual (Just r) newN
    teardownRelTests nodeFrom nodeTo r

-- | Test get relationship with unexisting id
case_GetUnexistingRelationshipById :: Assertion
case_GetUnexistingRelationshipById = withAuthConnection host port creds $ do
    newN <- getRelationship ("unexistingrelationship" :: S.ByteString)
    neo4jEqual Nothing newN
        
-- | Test delete relationship by id
case_DeleteRelationshipById :: Assertion
case_DeleteRelationshipById = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    deleteRelationship (relId r)
    teardownRelTests nodeFrom nodeTo r

-- | Test delete unexisting relationship by id
case_DeleteUnexistingRelationshipById :: Assertion
case_DeleteUnexistingRelationshipById = withAuthConnection host port creds $ 
        deleteRelationship ("unexistingrelationship" :: S.ByteString)

-- | Refresh relationship properties from the DB
case_getRelationshipProperties :: Assertion
case_getRelationshipProperties = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    props <- getProperties r
    neo4jEqual someProperties props
    teardownRelTests nodeFrom nodeTo r

-- | Get relationship properties from a deleted node
case_getDeletedRelationshipProperties :: Assertion
case_getDeletedRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        deleteNode nodeFrom
        deleteNode nodeTo
        getProperties r

-- | Get a property from a relationship
case_getRelationshipProperty :: Assertion
case_getRelationshipProperty = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    prop <- getProperty r "intarray"
    neo4jEqual (M.lookup "intarray" someProperties) prop
    teardownRelTests nodeFrom nodeTo r

-- | Get an unexisting property from a relationship
case_getRelationshipUnexistingProperty :: Assertion
case_getRelationshipUnexistingProperty = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    prop <- getProperty r "noproperty"
    neo4jEqual Nothing prop
    teardownRelTests nodeFrom nodeTo r

-- | Get a property from an unexisting relationship
case_getUnexistingRelationshipProperty :: Assertion
case_getUnexistingRelationshipProperty = do
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
    assertException expException $ withAuthConnection host port creds $ do
        teardownRelTests nodeFrom nodeTo r
        _ <- getProperty r "noproperty"
        return ()

-- | Get change the properties of a relationship
case_changeRelationshipProperties :: Assertion
case_changeRelationshipProperties = withAuthConnection host port creds $ do
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
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        deleteNode nodeFrom
        deleteNode nodeTo
        setProperties r someProperties

-- | Change a property of a relationship
case_changeRelationshipProperty :: Assertion
case_changeRelationshipProperty = withAuthConnection host port creds $ do
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
case_changeRelationshipPropertyToEmpty = withAuthConnection host port creds $ do
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
case_changeRelationshipUnexistingProperty = withAuthConnection host port creds $ do
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
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        _ <- setProperty r otherValName otherVal
        return ()
    withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r
    where otherValName = "mynewbool"
          otherVal = newval False

-- | Delete relationship properties
case_deleteRelationshipProperties :: Assertion
case_deleteRelationshipProperties = withAuthConnection host port creds $ do
    (nodeFrom, nodeTo, r) <- setupRelTests
    newN <- deleteProperties r
    neo4jEqual M.empty (getRelProperties newN)
    renewN <- getRelationship r
    neo4jEqual M.empty (getRelProperties $ fromJust renewN)
    teardownRelTests nodeFrom nodeTo newN

-- | Delete unexisting relationship properties
case_deleteUnexistingRelationshipProperties :: Assertion
case_deleteUnexistingRelationshipProperties = do
    (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
    let expException = Neo4jNoEntityException $ runRelIdentifier r 
    assertException expException $ withAuthConnection host port creds $ do
        deleteRelationship r
        _ <- deleteProperties r
        return ()
    withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r

-- | Delete a relationship property
case_deleteRelationshipProperty :: Assertion
case_deleteRelationshipProperty = withAuthConnection host port creds $ do
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
case_deleteRelationshipUnexistingProperty = withAuthConnection host port creds $ do
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
        (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
        let expException = Neo4jNoEntityException $ runRelIdentifier r 
        assertException expException $ withAuthConnection host port creds $ do
            deleteRelationship r
            _ <- deleteProperty r valName
            return ()
        withAuthConnection host port creds $ teardownRelTests nodeFrom nodeTo r
    where valName = "noproperty"

-- | Get relationships for a node
case_GetNodeRelationships :: Assertion
case_GetNodeRelationships = withAuthConnection host port creds $ do
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
        (nodeFrom, nodeTo, r) <- withAuthConnection host port creds setupRelTests
        let expException = Neo4jNoEntityException $ runNodeIdentifier nodeFrom
        assertException expException $ withAuthConnection host port creds $ do
            teardownRelTests nodeFrom nodeTo r
            _ <- getRelationships nodeFrom Any []
            return ()

-- | Get relationships for a node with multiple relationships
case_GetNodeRelationshipsMultiple :: Assertion
case_GetNodeRelationshipsMultiple = withAuthConnection host port creds $ do
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
case_GetNodeRelationshipsWithType = withAuthConnection host port creds $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        rsAny <- getRelationships nodeFrom Any [myRelType]
        neo4jEqual [r] rsAny
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for a node with an unexisting type filter
case_GetNodeRelationshipsWithUnexistingType :: Assertion
case_GetNodeRelationshipsWithUnexistingType = withAuthConnection host port creds $ do
        (nodeFrom, nodeTo, r) <- setupRelTests
        rsAny <- getRelationships nodeFrom Any ["notype"]
        neo4jEqual [] rsAny
        teardownRelTests nodeFrom nodeTo r

-- | Get relationships for a node with a type filter with multiple elements
case_GetNodeRelationshipsWithTypes :: Assertion
case_GetNodeRelationshipsWithTypes = withAuthConnection host port creds $ do
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
case_getLabels = withAuthConnection host port creds $ do
        n <- createNode someProperties
        addLabels [lbl] n
        lbls <- allLabels
        neo4jBool $ lbl `elem` lbls
        deleteNode n
    where lbl = "label1"

-- | Get labels for a node it doesn't have any
case_getNodeLabelsWithNone :: Assertion
case_getNodeLabelsWithNone = withAuthConnection host port creds $ do
    n <- createNode someProperties
    lbls <- getLabels n
    neo4jEqual [] lbls
    deleteNode n

-- | Get labels for an unexisting node
case_getUnexistingNodeLabels :: Assertion
case_getUnexistingNodeLabels = do
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        _ <- getLabels n
        return ()

-- | Add labels to a node and get them
case_getAddAndGetNodeLabels :: Assertion
case_getAddAndGetNodeLabels = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        addLabels ["mylabel"] n

-- | Change node labels
case_changeNodeLabels :: Assertion
case_changeNodeLabels = withAuthConnection host port creds $ do
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
case_changeNodeLabelsToEmpty = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        changeLabels ["mylabel"] n

-- | Remove a label from a node
case_removeNodeLabel :: Assertion
case_removeNodeLabel = withAuthConnection host port creds $ do
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
case_removeNodeUnexistingLabel = withAuthConnection host port creds $ do
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
    n <- withAuthConnection host port creds $ createNode someProperties
    let expException = Neo4jNoEntityException $ runNodeIdentifier n
    assertException expException $ withAuthConnection host port creds $ do
        deleteNode n
        removeLabel "mylabel" n

-- | Get all nodes with a label
case_allNodesWithLabel :: Assertion
case_allNodesWithLabel = withAuthConnection host port creds $ do
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
case_allNodesWithLabelAndProperty = withAuthConnection host port creds $ do
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
case_createGetDropIndex = withAuthConnection host port creds $ do
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
case_batchCreateGetNode = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                        n <- B.createNode someProperties
                        B.getNode n
                neo4jEqual 1 (length $ G.getNodes g)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g)
                deleteNode (head $ G.getNodes g)

-- | Test batch, create two nodes in a batch
case_batchCreate2Nodes :: Assertion
case_batchCreate2Nodes = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                        _ <- B.createNode someProperties
                        B.createNode anotherProperties
                neo4jEqual 2 (length $ G.getNodes g)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g)
                neo4jBool $ anotherProperties `elem` map getNodeProperties (G.getNodes g)
                mapM_ deleteNode (G.getNodes g)

-- | Test batch, create and delete
case_batchCreateDeleteNode :: Assertion
case_batchCreateDeleteNode = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                        n <- B.createNode someProperties
                        B.deleteNode n
                neo4jEqual 0 (length $ G.getNodes g)

-- | Test batch, create two nodes and two relationships between them
case_batchCreateRelationships :: Assertion
case_batchCreateRelationships = withAuthConnection host port creds $ do
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
case_batchCreateDelRelationships = withAuthConnection host port creds $ do
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
case_batchRelationshipNodeFrom = withAuthConnection host port creds $ do
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
case_batchRelationshipNodeTo = withAuthConnection host port creds $ do
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
case_batchGetRelationships = withAuthConnection host port creds $ do
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
case_batchSetProperties = withAuthConnection host port creds $ do
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
case_batchSetProperty = withAuthConnection host port creds $ do
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
case_batchDeleteProperties = withAuthConnection host port creds $ do
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
case_batchDeleteProperty= withAuthConnection host port creds $ do
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
        n <- withAuthConnection host port creds $ createNode someProperties
        let excMsg = "Node[" <> TE.decodeUtf8 (nodeId n) <>"] does not have a property \"" <> prop <> "\""
        let expException = Neo4jNoSuchProperty excMsg
        assertException expException $ withAuthConnection host port creds $ do
                    g <- B.runBatch $ B.deleteProperty n prop
                    neo4jEqual G.empty g -- Doing this to force evaluation
        withAuthConnection host port creds $ deleteNode n
    where prop = "noprop" :: T.Text

-- | Test batch, set add labels
case_batchAddLabels :: Assertion
case_batchAddLabels = withAuthConnection host port creds $ do
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
case_batchChangeLabels = withAuthConnection host port creds $ do
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
case_batchRemoveLabel = withAuthConnection host port creds $ do
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
case_batchAllNodesWithLabel = withAuthConnection host port creds $ do
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
case_batchAllNodesWithLabelAndProperty = withAuthConnection host port creds $ do
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

-- | Test cypher basic test
case_cypherBasic :: Assertion
case_cypherBasic = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            n1 <- B.createNode $ M.fromList ["name" |: ("I" :: T.Text)]
            n2 <- B.createNode $ M.fromList ["name" |: ("you" :: T.Text)]
            B.createRelationship "know" M.empty n1 n2
        -- actual test
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        neo4jEqual ["TYPE(r)"] (C.cols $ C.fromSuccess res)
        neo4jBool $ ["know"] `elem` (C.vals $ C.fromSuccess res)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x {name: {startName}})-[r]-(friend) WHERE friend.name = {name} RETURN TYPE(r)"
          params = M.fromList [("startName", C.newparam ("I" :: T.Text)), ("name", C.newparam ("you" :: T.Text))]


-- | Test cypher create a node
case_cypherCreateNode :: Assertion
case_cypherCreateNode = withAuthConnection host port creds $ do
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        let gp = G.addCypher (C.fromSuccess res) G.empty
        let props = M.fromList ["name" |: ("Andres" :: T.Text)]
        neo4jEqual 1 (length $ G.getNodes gp)
        neo4jBool $ props `elem` map getNodeProperties (G.getNodes gp)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "CREATE (n:Person { name : {name} }) RETURN n"
          params = M.fromList [("name", C.newparam ("Andres" :: T.Text))]

-- | Test cypher create a node with multiple properties
case_cypherCreateNodeMultipleProperties :: Assertion
case_cypherCreateNodeMultipleProperties = withAuthConnection host port creds $ do
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        let gp = G.addCypher (C.fromSuccess res) G.empty
        neo4jEqual 1 (length $ G.getNodes gp)
        neo4jBool $ props `elem` map getNodeProperties (G.getNodes gp)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "CREATE (n:Person { props }) RETURN n"
          props = M.fromList ["name" |: ("Michael" :: T.Text), "position" |: ("Developer" :: T.Text),
                              "awesome" |: True, "children" |: (3 :: Int64)]
          params = M.fromList [("props", C.ParamProperties props)]

-- | Test cypher create multiple nodes
case_cypherCreateMultipleNodes :: Assertion
case_cypherCreateMultipleNodes = withAuthConnection host port creds $ do
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        let gp = G.addCypher (C.fromSuccess res) G.empty
        neo4jEqual 2 (length $ G.getNodes gp)
        neo4jBool $ propsA `elem` map getNodeProperties (G.getNodes gp)
        neo4jBool $ propsB `elem` map getNodeProperties (G.getNodes gp)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "CREATE (n:Person { props }) RETURN n"
          propsA = M.fromList ["name" |: ("Andres" :: T.Text), "position" |: ("Developer" :: T.Text)]
          propsB = M.fromList ["name" |: ("Michael" :: T.Text), "position" |: ("Developer" :: T.Text)]
          params = M.fromList [("props", C.ParamPropertiesArray [propsA, propsB])]

-- | Test cypher set all properties on a node
case_cypherSetAllNodeProperties :: Assertion
case_cypherSetAllNodeProperties = withAuthConnection host port creds $ do
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        let gp = G.addCypher (C.fromSuccess res) G.empty
        neo4jEqual 1 (length $ G.getNodes gp)
        neo4jBool $ props `elem` map getNodeProperties (G.getNodes gp)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "CREATE (n:Person { name: 'this property is to be deleted' } ) SET n = { props } RETURN n"
          props = M.fromList ["name" |: ("Michael" :: T.Text), "position" |: ("Developer" :: T.Text),
                              "awesome" |: True, "children" |: (3 :: Int64)]
          params = M.fromList [("props", C.ParamProperties props)]

-- | Test cypher basic query
case_cypherBasicQuery :: Assertion
case_cypherBasicQuery = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            n1 <- B.createNode $ M.fromList ["name" |: ("I" :: T.Text)]
            n2 <- B.createNode $ M.fromList ["name" |: ("you" :: T.Text)]
            n3 <- B.createNode $ M.fromList ["name" |: ("him" :: T.Text), "age" |: (25 :: Int64)]
            _ <- B.createRelationship "know" M.empty n1 n2
            B.createRelationship "know" M.empty n1 n3
        -- actual test
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        neo4jEqual ["type(r)", "n.name", "n.age"] (C.cols $ C.fromSuccess res)
        neo4jBool $ ["know", "him", J.Number 25] `elem` (C.vals $ C.fromSuccess res)
        neo4jBool $ ["know", "you", J.Null] `elem` (C.vals $ C.fromSuccess res)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x {name: 'I'})-[r]->(n) RETURN type(r), n.name, n.age"
          params = M.empty

-- | Test cypher basic query get relationships
case_cypherBasicQueryGetRels :: Assertion
case_cypherBasicQueryGetRels = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            n1 <- B.createNode $ M.fromList ["name" |: ("I" :: T.Text)]
            n2 <- B.createNode $ M.fromList ["name" |: ("you" :: T.Text)]
            n3 <- B.createNode $ M.fromList ["name" |: ("him" :: T.Text), "age" |: (25 :: Int64)]
            _ <- B.createRelationship "know" M.empty n1 n2
            B.createRelationship "know" M.empty n1 n3
        -- actual test
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        let gres = G.addCypher (C.fromSuccess res) G.empty
        neo4jBool $ (length $ G.getRelationships gres) >= 2
        neo4jBool $ all (\r -> getRelType r == "know") (G.getRelationships gres)
        neo4jEqual ["r"] (C.cols $ C.fromSuccess res)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x {name: 'I'})-[r]->(n) RETURN r"
          params = M.empty

-- | Test cypher nested results
case_cypherNestedResults :: Assertion
case_cypherNestedResults = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            n1 <- B.createNode $ M.fromList ["name" |: ("I" :: T.Text)]
            n2 <- B.createNode $ M.fromList ["name" |: ("you" :: T.Text)]
            B.createRelationship "know" M.empty n1 n2
        -- actual test
        res <- C.cypher query params
        neo4jBool $ C.isSuccess res
        neo4jEqual ["collect(n.name)"] (C.cols $ C.fromSuccess res)
        let v = case (head $ head $ C.vals $ C.fromSuccess res) of
                 J.Array vec -> vec
                 _ -> V.empty
        neo4jBool $ "you" `V.elem` v
        neo4jBool $ "I" `V.elem` v
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (n) WHERE n.name in ['I', 'you'] RETURN collect(n.name)"
          params = M.empty

-- | Test cypher errors
case_cypherError :: Assertion
case_cypherError = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            B.createNode $ M.fromList ["age" |: (26 :: Int64)]
        -- actual test
        res <- C.cypher query params
        neo4jBool $ res `elem` [Left "BadInputException", Left "ArithmeticException"]
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x) WHERE has(x.age) RETURN x.age / 0"
          params = M.empty

-- | Test lone transaction cypher basic query
case_loneTransactionBasicQuery :: Assertion
case_loneTransactionBasicQuery = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ do
            n1 <- B.createNode $ M.fromList ["name" |: ("I" :: T.Text)]
            n2 <- B.createNode $ M.fromList ["name" |: ("you" :: T.Text)]
            n3 <- B.createNode $ M.fromList ["name" |: ("him" :: T.Text), "age" |: (25 :: Int64)]
            _ <- B.createRelationship "know" M.empty n1 n2
            B.createRelationship "know" M.empty n1 n3
        -- actual test
        res <- TC.loneQuery query params
        neo4jBool $ TC.isSuccess res
        neo4jEqual ["type(r)", "n.name", "n.age"] (TC.cols $ TC.fromSuccess res)
        neo4jBool $ ["know", "him", J.Number 25] `elem` (TC.vals $ TC.fromSuccess res)
        neo4jBool $ ["know", "you", J.Null] `elem` (TC.vals $ TC.fromSuccess res)
        neo4jEqual TC.emptyStats (TC.stats $ TC.fromSuccess res)
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x {name: 'I'})-[r]->(n) RETURN type(r), n.name, n.age"
          params = M.empty

-- | Test cypher errors in a transaction
case_cypherTransactionError :: Assertion
case_cypherTransactionError = withAuthConnection host port creds $ do
        -- create initial data
        gp <- B.runBatch $ B.createNode $ M.fromList ["age" |: (26 :: Int64)]
        -- actual test
        res <- TC.loneQuery query params
        neo4jEqual (Left ("Neo.ClientError.Statement.ArithmeticError","/ by zero")) res
        -- cleanup
        _ <- B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
        return ()
    where query = "MATCH (x) WHERE has(x.age) RETURN x.age / 0"
          params = M.empty

-- | Test a cypher transaction
case_cypherTransaction :: Assertion
case_cypherTransaction = withAuthConnection host port creds $ do
        res <- TC.runTransaction $ do
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            voidIO $ assertEqual "" ["pere", "pau", "p1", "pere.age"] (TC.cols result)
            voidIO $ assertEqual "" (TC.Stats True 2 0 2 1 0 2 0 0 0 0 0) (TC.stats result)
            let vals = TC.vals result
            voidIO $ assertEqual "" 1 (length vals)
            voidIO $ assertEqual "" 4 (length $ head vals)
            voidIO $ assertEqual "" (J.Number 78.0) (head vals !! 3)
            voidIO $ assertEqual "" 1 (length $ TC.graph result)
            voidIO $ assertEqual "" 1 (length $ G.getRelationships (head $ TC.graph result))
            voidIO $ assertEqual "" 2 (length $ G.getNodes (head $ TC.graph result))
            return result
        neo4jBool $ TC.isSuccess res
        let (Right tresult) = res
        let gp = head $ TC.graph tresult
        void $ B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        void $ B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
    where voidIO x = void $ liftIO x

-- | Test a cypher transaction with a explicit commit
case_cypherTransactionExplicitCommit :: Assertion
case_cypherTransactionExplicitCommit = withAuthConnection host port creds $ do
        res <- TC.runTransaction $ do
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            void $ TC.cypher "CREATE (pep: PERSON {age: 55})" M.empty
            TC.commit
            return result
        neo4jBool $ TC.isSuccess res
        let (Right result) = res
        voidIO $ assertEqual "" ["pere", "pau", "p1", "pere.age"] (TC.cols result)
        voidIO $ assertEqual "" (TC.Stats True 2 0 2 1 0 2 0 0 0 0 0) (TC.stats result)
        let vals = TC.vals result
        voidIO $ assertEqual "" 1 (length vals)
        voidIO $ assertEqual "" 4 (length $ head vals)
        voidIO $ assertEqual "" (J.Number 78.0) (head vals !! 3)
        voidIO $ assertEqual "" 1 (length $ TC.graph result)
        voidIO $ assertEqual "" 1 (length $ G.getRelationships (head $ TC.graph result))
        voidIO $ assertEqual "" 2 (length $ G.getNodes (head $ TC.graph result))
        let gp = head $ TC.graph result
        void $ B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        void $ B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
    where voidIO x = void $ liftIO x

-- | Test a cypher transaction with a keepalive
case_cypherTransactionKeepAlive :: Assertion
case_cypherTransactionKeepAlive = withAuthConnection host port creds $ do
        res <- TC.runTransaction $ do
            TC.keepalive
            void $ TC.cypher "CREATE (pep: PERSON {age: 55})" M.empty
            TC.keepalive
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            return result
        neo4jBool $ TC.isSuccess res
        let (Right result) = res
        voidIO $ assertEqual "" ["pere", "pau", "p1", "pere.age"] (TC.cols result)
        voidIO $ assertEqual "" (TC.Stats True 2 0 2 1 0 2 0 0 0 0 0) (TC.stats result)
        let vals = TC.vals result
        voidIO $ assertEqual "" 1 (length vals)
        voidIO $ assertEqual "" 4 (length $ head vals)
        voidIO $ assertEqual "" (J.Number 78.0) (head vals !! 3)
        voidIO $ assertEqual "" 1 (length $ TC.graph result)
        voidIO $ assertEqual "" 1 (length $ G.getRelationships (head $ TC.graph result))
        voidIO $ assertEqual "" 2 (length $ G.getNodes (head $ TC.graph result))
        let gp = head $ TC.graph result
        void $ B.runBatch $ mapM_ B.deleteRelationship (G.getRelationships gp)
        void $ B.runBatch $ mapM_ B.deleteNode (G.getNodes gp)
    where voidIO x = void $ liftIO x

-- | Test a cypher transaction with an error
case_cypherErrorInTransaction :: Assertion
case_cypherErrorInTransaction = withAuthConnection host port creds $ do
        res <- TC.runTransaction $ do
            -- query with wrong syntax
            void $ TC.cypher "i" M.empty
            TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
        neo4jBool $ not $ TC.isSuccess res
        neo4jEqual (Left ("Neo.ClientError.Statement.InvalidSyntax", "")) (procRes res)
    where procRes r@(Right _) = r
          procRes r@(Left (err, _))= Left (err, "")

-- | Test a cypher rollback transaction
case_cypherTransactionRollback :: Assertion
case_cypherTransactionRollback = withAuthConnection host port creds $ do
         void $ TC.runTransaction $ do
            void $ TC.cypher "CREATE (pepe: PERSON {age: 55})" M.empty
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            TC.rollback
            return result

-- | Test a cypher rollback and leave transaction
case_cypherTransactionRollbackAndLeave :: Assertion
case_cypherTransactionRollbackAndLeave = withAuthConnection host port creds $ do
         res <- TC.runTransaction $ do
            void $ TC.cypher "CREATE (pepe: PERSON {age: 55})" M.empty
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            TC.rollbackAndLeave "My message"
            TC.commit
            TC.rollback
            return result
         neo4jEqual (Left ("Rollback", "My message")) res

-- | Test issuing a commits after a rollback
case_cypherTransactionEnded :: Assertion
case_cypherTransactionEnded = assertException expException $ withAuthConnection host port creds $ do
     void $ TC.runTransaction $ do
            void $ TC.cypher "CREATE (pepe: PERSON {age: 55})" M.empty
            result <- TC.cypher "CREATE (pere: PERSON {age: {age}}) CREATE (pau: PERSON {props}) \
                              \CREATE p1 = (pere)-[:KNOWS]->(pau) RETURN pere, pau, p1, pere.age" $
                                M.fromList [("age", TC.newparam (78 :: Int64)),
                                            ("props", TC.ParamProperties $ M.fromList["age" |: (99 :: Int64)])]
            TC.rollback
            TC.commit
            return result
    where expException = TransactionEndedExc

-- | Test graph union op
case_graphUnion :: Assertion
case_graphUnion = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    void $ B.addLabels ["a"] n1
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                g2 <- B.runBatch $ do
                    n1 <- B.createNode someOtherProperties
                    n2 <- B.createNode anotherProperties
                    void $ B.addLabels ["b"] n1
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                let g3 = g `G.union` g2
                neo4jEqual 4 (length $ G.getNodes g3)
                neo4jEqual 4 (length $ G.getRelationships g3)
                neo4jBool $ someOtherProperties `elem` map getRelProperties (G.getRelationships g3)
                neo4jBool $ someProperties `elem` map getRelProperties (G.getRelationships g3)
                neo4jBool $ someOtherProperties `elem` map getNodeProperties (G.getNodes g3)
                neo4jBool $ someProperties `elem` map getNodeProperties (G.getNodes g3)
                neo4jBool $ "type1" `elem` map getRelType (G.getRelationships g3)
                neo4jBool $ "type2" `elem` map getRelType (G.getRelationships g3)
                neo4jBool $ "a" `elem` (join $ map (HS.toList . (flip G.getNodeLabels g3)) (G.getNodes g3))
                neo4jBool $ "b" `elem` (join $ map (HS.toList . (flip G.getNodeLabels g3)) (G.getNodes g3))
                mapM_ deleteRelationship (G.getRelationships g3)
                mapM_ deleteNode (G.getNodes g3)

-- | Test graph difference op
case_graphDifference :: Assertion
case_graphDifference = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    void $ B.addLabels ["a"] n1
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                let g2 = g `G.difference` g
                neo4jEqual 0 (length $ G.getRelationships g2)
                neo4jEqual 0 (length $ G.getNodes g2)
                let subGraph = G.deleteNode (head $ G.getNodes g) g
                let g3 = g `G.difference` (G.deleteRelationship (head $ G.getRelationships subGraph) subGraph)
                neo4jEqual 1 (length $ G.getRelationships g3)
                neo4jEqual 1 (length $ G.getNodes g3)

-- | Test graph intersection op
case_graphIntersection :: Assertion
case_graphIntersection = withAuthConnection host port creds $ do
                g <- B.runBatch $ do
                    n1 <- B.createNode someProperties
                    n2 <- B.createNode anotherProperties
                    void $ B.addLabels ["a"] n1
                    _ <- B.createRelationship "type1" someOtherProperties n1 n2
                    B.createRelationship "type2" someProperties n2 n1
                g2 <- B.runBatch $ do
                    B.createNode someProperties
                let g3 = G.addNode (head $ G.getNodes g) g2
                let g4 = g `G.intersection` (G.addRelationship (head $ G.getRelationships g) g3)
                neo4jEqual 1 (length $ G.getRelationships g4)
                neo4jEqual 1 (length $ G.getNodes g4)
                neo4jEqual (head $ G.getRelationships g) (head $ G.getRelationships g4)
                neo4jEqual (head $ G.getNodes g) (head $ G.getNodes g4)

-- | Environment for traversal tests
setUpTraversalTest :: Neo4j G.Graph
setUpTraversalTest = B.runBatch $ do
                            root <- _createNode "Root"
                            johan <- _createNode "Johan"
                            mattias <- _createNode "Mattias"
                            emil <- _createNode "Emil"
                            peter <- _createNode "Peter"
                            tobias <- _createNode "Tobias"
                            sara <- _createNode "Sara"
                            gumersindo <- _createNode "Gumersindo"
                            _createRel "Root-Johan" "knows" root johan
                            _createRel "Root-Mattias" "knows" root mattias 
                            _createRel "Johan-Emil" "knows" johan emil 
                            _createRel "Emil-Peter" "knows" emil peter
                            _createRel "Emil-Tobias" "knows" emil tobias
                            _createRel "Tobias-Sara" "loves" tobias sara
                            _createRel "Gumersindo-Tobias" "hates" gumersindo tobias
                            _createRel "Gumersindo-Sara" "admires" sara gumersindo
    where _createNode name = B.createNamedNode name $ M.fromList ["name" |: (fromString name :: T.Text)]
          _createRel name t from to = void $ B.createNamedRelationship name t M.empty from to

-- | clean up a traversal test
cleanUpTraversalTest :: G.Graph -> Neo4j ()
cleanUpTraversalTest g = void $ B.runBatch $ do
                            mapM_ B.deleteRelationship (G.getRelationships g)
                            mapM_ B.deleteNode (G.getNodes g)


-- | Test a traversal with an unexisting start
case_traversalNotFound :: Assertion
case_traversalNotFound = do
        g <- withAuthConnection host port creds $ setUpTraversalTest
        let start = fromJust $ G.getNamedNode "Root" g
        let expException = Neo4jNoEntityException $ (TE.encodeUtf8 . runNodePath . nodePath) start
        assertException expException $ withAuthConnection host port creds $ do
            let rels = map (\n -> fromJust $ G.getNamedRelationship n g) ["Root-Johan", "Root-Mattias"]
            mapM_ deleteRelationship rels
            deleteNode start
            void $ T.traverseGetNodes def start
        withAuthConnection host port creds $ cleanUpTraversalTest g

-- | Test a default traversal returning nodes
case_defaultTraversalNodes :: Assertion
case_defaultTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    ns <- T.traverseGetNodes def start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Root", "Mattias", "Johan"]
    neo4jEqual (L.sort expected) (L.sort ns)
    cleanUpTraversalTest g

-- | Test a default traversal returning relationships
case_defaultTraversalRels :: Assertion
case_defaultTraversalRels = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    rs <- T.traverseGetRels def start
    let expected = map (\n -> fromJust $ G.getNamedRelationship n g) ["Root-Mattias", "Root-Johan"]
    neo4jEqual (L.sort expected) (L.sort rs)
    cleanUpTraversalTest g

-- | Test a default traversal returning id paths
case_defaultTraversalIdPath :: Assertion
case_defaultTraversalIdPath = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    ps <- L.sort <$> T.traverseGetPath def start
    neo4jEqual 3 (length ps)
    -- check we've received the expected paths
    let _getNodePath = \name -> nodePath $ fromJust $ G.getNamedNode name g
    let _getRelPath = \name -> relPath $ fromJust $ G.getNamedRelationship name g
    let checkPath = \p nodes rels -> do
        neo4jEqual (map _getNodePath nodes) (T.pathNodes p)
        let resRels = T.pathRels p
        neo4jEqual (map _getRelPath (map fst rels)) (map fst resRels)
        neo4jEqual (map snd rels) (map snd resRels)
    checkPath (ps !! 0) ["Root"] []
    checkPath (ps !! 1) ["Root", "Johan"] [("Root-Johan", T.Out)]
    checkPath (ps !! 2) ["Root", "Mattias"] [("Root-Mattias", T.Out)]
    cleanUpTraversalTest g

-- | Test a default traversal returning full paths
case_defaultTraversalFullPath :: Assertion
case_defaultTraversalFullPath = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    ps <- L.sort <$> T.traverseGetFullPath def start
    neo4jEqual 3 (length ps)
    -- check we've received the expected paths
    let _getNode = \name -> fromJust $ G.getNamedNode name g
    let _getRel = \name -> fromJust $ G.getNamedRelationship name g
    let checkPath = \p nodes rels -> do
        neo4jEqual (L.sort $ map _getNode nodes) (L.sort $ T.pathNodes p)
        neo4jEqual (L.sort $ map _getRel rels) (L.sort $ T.pathRels p)
    let checkPath' = \p nodes rels -> (L.sort $ map _getNode nodes) == (L.sort $ T.pathNodes p) &&
                                      (L.sort $ map _getRel rels) == (L.sort $ T.pathRels p)
    checkPath (ps !! 0) ["Root"] []
    if checkPath' (ps !! 1) ["Root", "Johan"] ["Root-Johan"]
       then checkPath (ps !! 1) ["Root", "Johan"] ["Root-Johan"]
       else checkPath (ps !! 1) ["Root", "Mattias"] ["Root-Mattias"]
    if checkPath' (ps !! 2) ["Root", "Johan"] ["Root-Johan"]
       then checkPath (ps !! 2) ["Root", "Johan"] ["Root-Johan"]
       else checkPath (ps !! 2) ["Root", "Mattias"] ["Root-Mattias"]
    cleanUpTraversalTest g

-- | Test a traversal with an unexisting start
case_pagedTraversalNotFound :: Assertion
case_pagedTraversalNotFound = do
        g <- withAuthConnection host port creds $ setUpTraversalTest
        let start = fromJust $ G.getNamedNode "Root" g
        let expException = Neo4jNoEntityException $ (TE.encodeUtf8 . runNodePath . nodePath) start
        assertException expException $ withAuthConnection host port creds $ do
            let rels = map (\n -> fromJust $ G.getNamedRelationship n g) ["Root-Johan", "Root-Mattias"]
            mapM_ deleteRelationship rels
            deleteNode start
            void $ T.pagedTraverseGetNodes def def start
        withAuthConnection host port creds $ cleanUpTraversalTest g

-- | Test a paged traversal returning nodes
case_pagedTraversalNodes :: Assertion
case_pagedTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def {T.travDepth = Left 2}
    let paging = def {T.pageSize = 1}
    pg <- T.pagedTraverseGetNodes desc paging start
    let _getNode = \name -> fromJust $ G.getNamedNode name g
    neo4jEqual False (T.pagedTraversalDone pg)
    neo4jEqual [_getNode "Root"] (T.getPagedValues pg)
    pg2 <- T.nextTraversalPage pg
    neo4jEqual False (T.pagedTraversalDone pg2)
    pg3 <- T.nextTraversalPage pg2
    neo4jEqual False (T.pagedTraversalDone pg3)
    neo4jEqual (L.sort $ map _getNode ["Johan", "Mattias"]) (L.sort $ T.getPagedValues pg2 ++ T.getPagedValues pg3)
    pg4 <- T.nextTraversalPage pg3
    neo4jEqual False (T.pagedTraversalDone pg4)
    neo4jEqual [_getNode "Emil"] (T.getPagedValues pg4)
    pg5 <- T.nextTraversalPage pg4
    neo4jEqual True (T.pagedTraversalDone pg5)
    pg6 <- T.nextTraversalPage pg5
    neo4jEqual True (T.pagedTraversalDone pg6)
    cleanUpTraversalTest g

-- | Test a paged traversal returning relationships
case_pagedTraversalRels :: Assertion
case_pagedTraversalRels = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def
    let paging = def {T.pageSize = 1}
    pg <- T.pagedTraverseGetRels desc paging start
    let _getRel = \name -> fromJust $ G.getNamedRelationship name g
    neo4jEqual False (T.pagedTraversalDone pg)
    pg2 <- T.nextTraversalPage pg
    neo4jEqual False (T.pagedTraversalDone pg2)
    pg3 <- T.nextTraversalPage pg2
    neo4jEqual False (T.pagedTraversalDone pg3)
    neo4jEqual (L.sort $ map _getRel ["Root-Johan", "Root-Mattias"]) (
        L.sort $ T.getPagedValues pg ++ T.getPagedValues pg2 ++ T.getPagedValues pg3)
    pg4 <- T.nextTraversalPage pg3
    neo4jEqual True (T.pagedTraversalDone pg4)
    cleanUpTraversalTest g

-- | Test a paged traversal returning id paths 
case_pagedTraversalIdPaths :: Assertion
case_pagedTraversalIdPaths = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def
    let paging = def {T.pageSize = 1}
    let _getNodePath = \name -> nodePath $ fromJust $ G.getNamedNode name g
    let _getRelPath = \name -> relPath $ fromJust $ G.getNamedRelationship name g
    let checkPath = \p nodes rels -> do
        neo4jEqual (map _getNodePath nodes) (T.pathNodes p)
        let resRels = T.pathRels p
        neo4jEqual (map _getRelPath (map fst rels)) (map fst resRels)
        neo4jEqual (map snd rels) (map snd resRels)
    pg <- T.pagedTraverseGetPath desc paging start
    neo4jEqual False (T.pagedTraversalDone pg)
    checkPath (T.getPagedValues pg !! 0) ["Root"] []
    pg2 <- T.nextTraversalPage pg
    neo4jEqual False (T.pagedTraversalDone pg2)
    pg3 <- T.nextTraversalPage pg2
    neo4jEqual False (T.pagedTraversalDone pg3)
    let ps = L.sort $ T.getPagedValues pg2 ++ T.getPagedValues pg3
    checkPath (ps !! 0) ["Root", "Johan"] [("Root-Johan", T.Out)]
    checkPath (ps !! 1) ["Root", "Mattias"] [("Root-Mattias", T.Out)]
    pg4 <- T.nextTraversalPage pg3
    neo4jEqual True (T.pagedTraversalDone pg4)
    cleanUpTraversalTest g

-- | Test a paged traversal returning full paths 
case_pagedTraversalFullPaths :: Assertion
case_pagedTraversalFullPaths = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def
    let paging = def {T.pageSize = 1}
    let _getNode = \name -> fromJust $ G.getNamedNode name g
    let _getRel = \name -> fromJust $ G.getNamedRelationship name g
    let checkPath = \p nodes rels -> do
        neo4jEqual (L.sort $ map _getNode nodes) (L.sort $ T.pathNodes p)
        neo4jEqual (L.sort $ map _getRel rels) (L.sort $ T.pathRels p)
    pg <- T.pagedTraverseGetFullPath desc paging start
    neo4jEqual False (T.pagedTraversalDone pg)
    checkPath (T.getPagedValues pg !! 0) ["Root"] []
    pg2 <- T.nextTraversalPage pg
    neo4jEqual False (T.pagedTraversalDone pg2)
    pg3 <- T.nextTraversalPage pg2
    neo4jEqual False (T.pagedTraversalDone pg3)
    let ps = L.sort $ T.getPagedValues pg2 ++ T.getPagedValues pg3
    checkPath (ps !! 0) ["Root", "Johan"] ["Root-Johan"]
    checkPath (ps !! 1) ["Root", "Mattias"] ["Root-Mattias"]
    pg4 <- T.nextTraversalPage pg3
    neo4jEqual True (T.pagedTraversalDone pg4)
    cleanUpTraversalTest g

-- | Test traversal depth first search
case_dfsTraversalNodes :: Assertion
case_dfsTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def {T.travDepth = Left 2, T.travOrder = T.DepthFirst}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Root", "Johan", "Emil", "Mattias"]
    neo4jEqual (L.sort expected) (L.sort ns)
    cleanUpTraversalTest g

-- | Test traversal with a relation filter
case_relFilterTraversalNodes :: Assertion
case_relFilterTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Tobias" g
    let desc = def {T.travRelFilter = [("loves", Outgoing)]}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara"]
    neo4jEqual (L.sort expected) (L.sort ns)
    let desc2 = def {T.travRelFilter = [("hates", Incoming)]}
    ns2 <- T.traverseGetNodes desc2 start
    let expected2 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Gumersindo"]
    neo4jEqual (L.sort expected2) (L.sort ns2)
    let desc3 = def {T.travRelFilter = [("hates", Any)]}
    ns3 <- T.traverseGetNodes desc3 start
    let expected3 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Gumersindo"]
    neo4jEqual (L.sort expected3) (L.sort ns3)
    let desc4 = def {T.travRelFilter = [("hates", Incoming), ("loves", Outgoing)]}
    ns4 <- T.traverseGetNodes desc4 start
    let expected4 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected4) (L.sort ns4)
    let desc5 = def {T.travRelFilter = [("htes", Incoming)]}
    ns5 <- T.traverseGetNodes desc5 start
    let expected5 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias"]
    neo4jEqual (L.sort expected5) (L.sort ns5)
    cleanUpTraversalTest g

-- | Test traversal with a uniqueness constraint
case_uniquenessTraversalNodes :: Assertion
case_uniquenessTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Tobias" g
    let desc = def {T.travDepth = Left 2, T.travRelFilter = [("loves", Any), ("hates", Any), ("admires", Any)]}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara", "Gumersindo", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected) (L.sort ns)
    let desc2 = desc {T.travUniqueness = Just T.NodeGlobal}
    ns2 <- T.traverseGetNodes desc2 start
    let expected2 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected2) (L.sort ns2)
    let desc3 = desc {T.travUniqueness = Just T.RelationshipGlobal}
    ns3 <- T.traverseGetNodes desc3 start
    let expected3 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Gumersindo", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected3) (L.sort ns3)
    let desc4 = desc {T.travUniqueness = Just T.NodePathUnique}
    ns4 <- T.traverseGetNodes desc4 start
    let expected4 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara", "Gumersindo", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected4) (L.sort ns4)
    let desc5 = desc {T.travUniqueness = Just T.RelationshipPath}
    ns5 <- T.traverseGetNodes desc5 start
    let expected5 = map (\n -> fromJust $ G.getNamedNode n g) ["Tobias", "Sara", "Gumersindo", "Sara", "Gumersindo"]
    neo4jEqual (L.sort expected5) (L.sort ns5)
    cleanUpTraversalTest g

-- | Test traversal with a javascript depth expression
case_progDepthTraversalNodes :: Assertion
case_progDepthTraversalNodes = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def {T.travDepth = Right "position.length() >= 2;"}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Root", "Johan", "Mattias", "Emil"]
    neo4jEqual (L.sort expected) (L.sort ns)
    cleanUpTraversalTest g

-- | Test traversal with a wrong javascript depth expression
case_wrongProgDepthTraversalNodes :: Assertion
case_wrongProgDepthTraversalNodes = do
        g <- withAuthConnection host port creds $ setUpTraversalTest
        assertException expException $ withAuthConnection host port creds $ do
            let start = fromJust $ G.getNamedNode "Root" g
            let desc = def {T.travDepth = Right "positionlength() >= 2;"}
            void $ T.traverseGetNodes desc start
        withAuthConnection host port creds $ cleanUpTraversalTest g
      where expException = Neo4jUnexpectedResponseException HT.status400

-- | Test traversal without initial node
case_returnButFirstTraversal :: Assertion
case_returnButFirstTraversal = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def {T.travNodeFilter = Left T.ReturnAllButStartNode}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Johan", "Mattias"]
    neo4jEqual (L.sort expected) (L.sort ns)
    cleanUpTraversalTest g

-- | Test filter nodes without initial node
case_filterNodesTraversal :: Assertion
case_filterNodesTraversal = withAuthConnection host port creds $ do
    g <- setUpTraversalTest
    let start = fromJust $ G.getNamedNode "Root" g
    let desc = def {T.travNodeFilter = Right "position.endNode().getProperty('name').toLowerCase().contains('t');"}
    ns <- T.traverseGetNodes desc start
    let expected = map (\n -> fromJust $ G.getNamedNode n g) ["Root", "Mattias"]
    neo4jEqual (L.sort expected) (L.sort ns)
    cleanUpTraversalTest g

-- | Test traversal with a filter expression
case_wrongFilterNodesTraversal :: Assertion
case_wrongFilterNodesTraversal = do
        g <- withAuthConnection host port creds $ setUpTraversalTest
        assertException expException $ withAuthConnection host port creds $ do
            let start = fromJust $ G.getNamedNode "Root" g
            let desc = def {T.travNodeFilter = Right "posiiiitionlength() >= 2;"}
            void $ T.traverseGetNodes desc start
        withAuthConnection host port creds $ cleanUpTraversalTest g
      where expException = Neo4jUnexpectedResponseException HT.status400
