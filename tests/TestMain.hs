{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mappend)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Control.Exception
import Control.Monad
import Data.Int
import Data.List
import Data.Maybe
import Distribution.Simple.Utils
import Distribution.Verbosity
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as B
import qualified Test.HUnit as H
import Network.HTTP.Conduit

import Database.Neo4j
import qualified Network.HTTP.Conduit as HC

(<>) :: String -> String -> String
(<>) = mappend

-- Get the string of a exception raised by an action
getException :: IO a -> IO (Maybe Neo4jException)
getException action = handle (return . Just) $ do
    _ <- action
    return Nothing

assertException :: (Exception e, Show e) => e -> IO a -> Assertion
assertException exExc action = do
        resExc <- getException action
        checkExc resExc
    where --checkExc :: (Exception e, Show e) => Maybe e -> Assertion
          checkExc Nothing = assertFailure noExcMsg
          checkExc (Just (Neo4jHttpException (HC.FailedConnectionException2 "localhost" _ _ ))) = do
            liftIO $ print $ fromException ee
            assertEqual "" (show exExc) (show ee)
          checkExc (Just e) = assertEqual "" (show exExc) (show e)
          noExcMsg = "Expected exception " <> show exExc <> " but none raised"
    

main :: IO ()
main = $(defaultMainGenerator)

case_NoConnection :: Assertion
case_NoConnection = do
     assertException expException $ withConnection "localhost" port $ do
        n <- createNode $ M.fromList ["hola" |: ("adeu" :: T.Text)]
        liftIO $ print n
    where expException = Neo4jHttpException (HC.FailedConnectionException2 "localhost" port False $ toException TooManyRetries)
          port = 77
