{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http
import Database.Neo4j.Node

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
