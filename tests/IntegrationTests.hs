{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mappend)
import Data.Int (Int64)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

import Control.Exception
import Test.Framework.TH
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
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
    

main :: IO ()
main = $(defaultMainGenerator)

-- TODO: Is it possible to avoid so many type declarations for literals when constructing properties??
-- | Dummy properties
someProperties :: Properties
someProperties = M.fromList ["text" |: ("mytext" :: T.Text),
                             "textarray" |: ["a" :: T.Text, "", "adeu"],
                             "int" |: (-12 :: Int64),
                             "intarray" |: [1 :: Int64, 2],
                             "double" |: (-12.23 :: Double),
                             "doublearray" |: [0.1, (-12.23 :: Double)],
                             "bool" |: False,
                             "boolarray" |: [False, True]
                            ]

-- | Default Neo4j port
port :: Port
port = 7474

-- | Default Neo4j host
host :: Hostname
host = "localhost"

-- | Test connecting to a non-existing server
case_NoConnection :: Assertion
case_NoConnection = assertException expException $ withConnection "localhost" 77 $ do
                                                        createNode someProperties
    where expException = Neo4jHttpException "FailedConnectionException2 \"localhost\" 77 False connect: does\
                                             \ not exist (Connection refused)"

-- | Test get and create a node
case_CreateGetDeleteNode :: Assertion
case_CreateGetDeleteNode = withConnection host port $ do
    n <- createNode someProperties
    newN <- getNode n
    liftIO $ assertEqual "" (Just n) newN
    delRes <- deleteNode n
    liftIO $ assertBool "Expected cleanup delete to be successful" delRes

-- | Test delete and get a node
case_CreateDeleteGetNode :: Assertion
case_CreateDeleteGetNode = withConnection host port $ do
    n <- createNode someProperties
    delRes <- deleteNode n
    liftIO $ assertBool "" delRes
    newN <- getNode n
    liftIO $ assertEqual "" Nothing newN

-- | Test double delete
case_DoubleDeleteNode :: Assertion
case_DoubleDeleteNode = withConnection host port $ do
    n <- createNode someProperties
    delRes <- deleteNode n
    liftIO $ assertBool "" delRes
    newDelRes <- deleteNode n
    liftIO $ assertBool "" newDelRes

-- | Test get node by id
case_GetNodeById :: Assertion
case_GetNodeById = withConnection host port $ do
    n <- createNode someProperties
    newN <- getNodeById (nodeId n)
    liftIO $ assertEqual "" (Just n) newN
    delRes <- deleteNode n
    liftIO $ assertBool "Expected cleanup delete to be successful" delRes

-- | Test get node with unexisting id
case_GetUnexistingNodeById :: Assertion
case_GetUnexistingNodeById = withConnection host port $ do
    newN <- getNodeById "unexistingnode"
    liftIO $ assertEqual "" Nothing newN
        
-- | Test delete node by id
case_DeleteNodeById :: Assertion
case_DeleteNodeById = withConnection host port $ do
    n <- createNode someProperties
    delRes <- deleteNodeById (nodeId n)
    liftIO $ assertBool "" delRes

-- | Test delete unexisting node by id
case_DeleteUnexistingNodeById :: Assertion
case_DeleteUnexistingNodeById = withConnection host port $ do
    delRes <- deleteNodeById "unexistingnode"
    liftIO $ assertBool "" delRes

-- | Refresh node properties from the DB
case_getNodeProperties :: Assertion
case_getNodeProperties = withConnection host port $ do
    n <- createNode someProperties
    props <- getProperties n
    liftIO $ assertEqual "" someProperties props
    delRes <- deleteNode n
    liftIO $ assertBool "Expected cleanup delete to be successful" delRes

-- | Get node properties from a deleted node
case_getDeletedNodeProperties :: Assertion
case_getDeletedNodeProperties = assertException expException $ withConnection host port $ do
        n <- createNode someProperties
        delRes <- deleteNode n
        liftIO $ assertBool "" delRes
        getProperties n
    where expException = Neo4jUnexpectedResponseException HT.status404

-- | Get a property from a node
case_getNodeProperty :: Assertion
case_getNodeProperty = withConnection host port $ do
    n <- createNode someProperties
    prop <- getProperty n "intarray"
    liftIO $ assertEqual "" (M.lookup "intarray" someProperties) prop
    delRes <- deleteNode n
    liftIO $ assertBool "" delRes

-- | Get an unexisting property from a node
case_getNodeUnexistingProperty :: Assertion
case_getNodeUnexistingProperty = withConnection host port $ do
    n <- createNode someProperties
    prop <- getProperty n "noproperty"
    liftIO $ assertEqual "" Nothing prop
    delRes <- deleteNode n
    liftIO $ assertBool "" delRes

-- | Get change the properties of a node
case_changeNodeProperties :: Assertion
case_changeNodeProperties = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperties n otherProperties
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.insert "newbool" (newval True) someProperties

-- | Get change the properties of a node that doesn't exist
case_changeUnexistingNodeProperties :: Assertion
case_changeUnexistingNodeProperties = assertException expException $ withConnection host port $ do
        n <- createNode someProperties
        delRes <- deleteNode n
        liftIO $ assertBool "" delRes
        setProperties n someProperties
    where expException = Neo4jUnexpectedResponseException HT.status404

-- | Change a property of a node
case_changeNodeProperty :: Assertion
case_changeNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "int"
          otherVal = newval False

-- | Change an array property of a node to empty
case_changeNodePropertyToEmpty :: Assertion
case_changeNodePropertyToEmpty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "intarray"
          otherVal = newval ([] :: [Int64])

-- | Set an unexisting property of a node
case_changeNodeUnexistingProperty :: Assertion
case_changeNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- setProperty n otherValName otherVal
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.insert otherValName otherVal someProperties
          otherValName = "mynewbool"
          otherVal = newval False

-- | Delete node properties
case_deleteNodeProperties :: Assertion
case_deleteNodeProperties = withConnection host port $ do
    n <- createNode someProperties
    newN <- deleteProperties n
    liftIO $ assertEqual "" M.empty (nodeProperties newN)
    renewN <- getNode n
    liftIO $ assertEqual "" M.empty (nodeProperties $ fromJust renewN)
    delRes <- deleteNode newN
    liftIO $ assertBool "" delRes

-- | Delete a node property
case_deleteNodeProperty :: Assertion
case_deleteNodeProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.delete valName someProperties
          valName = "int"

-- | Delete an unexisting property from a node
case_deleteNodeUnexistingProperty :: Assertion
case_deleteNodeUnexistingProperty = withConnection host port $ do
        n <- createNode someProperties
        newN <- deleteProperty n valName
        liftIO $ assertEqual "" otherProperties (nodeProperties newN)
        renewN <- getNode n
        liftIO $ assertEqual "" otherProperties (nodeProperties $ fromJust renewN)
        delRes <- deleteNode newN
        liftIO $ assertBool "" delRes
    where otherProperties = M.delete valName someProperties
          valName = "noproperty"

someOtherProperties :: Properties
someOtherProperties = M.fromList ["hola" |: ("adeu" :: T.Text), "proparray" |: ["a" :: T.Text, "", "adeu"]]

anotherProperties :: Properties
anotherProperties = M.empty

myRelType :: T.Text
myRelType = "MYREL"

-- | Test get and create a relationship
case_CreateGetDeleteRelationship :: Assertion
case_CreateGetDeleteRelationship = withConnection host port $ do
    nodeFrom <- createNode someProperties
    nodeTo <- createNode someOtherProperties
    r <- createRelationship myRelType anotherProperties nodeFrom nodeTo
    newN <- getRelationship r
    liftIO $ assertEqual "" (Just r) newN
    deleteRelationship r
    delFrom <- deleteNode nodeFrom
    delTo <- deleteNode nodeTo
    liftIO $ assertBool "Expected cleanup delete to be successful" (all (\x -> x) [delFrom, delTo])

