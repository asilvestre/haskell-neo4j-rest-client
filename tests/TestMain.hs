{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mappend)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

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

-- | Dummy properties
someProperties :: Properties
someProperties = M.fromList ["hola" |: ("adeu" :: T.Text)]

-- | Test connecting to a non-existing server
case_NoConnection :: Assertion
case_NoConnection = assertException expException $ withConnection "localhost" 77 $ do
                                                        createNode someProperties
    where expException = Neo4jHttpException "FailedConnectionException2 \"localhost\" 77 False connect: does\
                                             \ not exist (Connection refused)"

-- | Default Neo4j port
port :: Port
port = 7474

-- | Default Neo4j host
host :: Hostname
host = "localhost"

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
