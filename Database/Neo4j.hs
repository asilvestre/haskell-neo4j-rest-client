{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j () where

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Default (def)
import Data.Int (Int64)

import qualified Data.Aeson as J
import qualified Data.Attoparsec.Number as AttoNum
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT


data Val = IntVal Int64 | BoolVal Bool | TextVal T.Text | DoubleVal Double deriving (Show)
data PropertyValue =  ValueProperty Val | ArrayProperty [Val] deriving (Show)

instance J.ToJSON Val where
    toJSON (IntVal v)  = J.Number $ fromIntegral v
    toJSON (BoolVal v)  = J.Bool v
    toJSON (TextVal v)  = J.String v
    toJSON (DoubleVal v)  = J.Number $ Sci.fromFloatDigits v

instance J.ToJSON PropertyValue where
    toJSON (ValueProperty v) = J.toJSON v
    toJSON (ArrayProperty vs) = J.Array (V.fromList $ map J.toJSON vs) 

instance J.FromJSON PropertyValue where
    parseJSON (J.Number (AttoNum.I v)) = IntVal v
    parseJSON (J.Number (AttoNum.D v)) = DoubleVal v
    parseJSON (J.Bool v) = BoolVal v
    parseJSON (J.String v) = TextVal v
    parseJSON (J.Array v) =  ArrayProperty $ map J.parseJSON (V.toList v)
    parseJSON _ = mzero


type Properties = M.HashMap T.Text PropertyValue

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


createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn ->  do
                        (headers, body) <- httpReq conn "POST" "/db/data/node" ""
                        return $ Node "Myloc"

createNodes :: Neo4j Node
createNodes = do
        createNode M.empty
        createNode M.empty
