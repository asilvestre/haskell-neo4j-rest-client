{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j () where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Default (def)
import Data.Int (Int64)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT


data Val = IntVal Int64 | BoolVal Bool | TextVal T.Text | DoubleVal Double
data PropertyValue =  ValueProperty Val | ArrayProperty [Val]

type PropertyIndex = M.Map T.Text PropertyValue

data Node = Node {nodeLocation :: L.ByteString}

data Relationship = Relationship

newtype Label = Label {runLabel :: T.Text}


data Connection = Connection {dbHostname :: Hostname, dbPort :: Port, manager :: HC.Manager}

type Hostname = S.ByteString
type Port = Int


newConnection :: Hostname -> Port -> ResourceT IO Connection
newConnection hostname port = do
        manager <- liftIO $ HC.newManager HC.conduitManagerSettings
        return $ Connection hostname port manager


withConnection :: Hostname -> Port -> IO Connection
withConnection hostname port = runResourceT $ newConnection hostname port


newtype Neo4j a = Neo4j { runNeo4j :: Connection -> ResourceT IO a }


instance Monad Neo4j where
    return x = Neo4j (const (return x))
    (Neo4j cmd) >>= f = Neo4j $ \con -> do
                            a <- cmd con
                            runNeo4j (f a) con


neo4j :: Connection -> Neo4j a -> ResourceT IO a
neo4j con n = runNeo4j n con


httpReq :: Connection -> HT.Method -> S.ByteString -> L.ByteString -> ResourceT IO (HT.ResponseHeaders, L.ByteString)
httpReq (Connection h p m) method path body = do
                                let request = def {
                                        HC.host = h,
                                        HC.port = p,
                                        HC.path = path,
                                        HC.method = method,
                                        HC.requestBody = HC.RequestBodyLBS body,
                                        HC.requestHeaders = [(HT.hAccept, "application/json; charset=UTF-8"),
                                                              (HT.hContentType, "application/json")]}
                                res <- HC.httpLbs request m
                                return (HC.responseHeaders res, HC.responseBody res)


createNode :: PropertyIndex -> Neo4j Node
createNode props = Neo4j $ \conn ->  do
                        (headers, body) <- httpReq conn "POST" "/db/data/node" ""
                        return Node
