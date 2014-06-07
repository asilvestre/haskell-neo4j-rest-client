{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Database.Neo4j () where

import Data.Monoid (mappend)
import Data.Typeable (Typeable)
import Control.Exception.Base (Exception, throw, catch, toException)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Default (def)
import Data.Int (Int64)

import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as M
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

(<>) = mappend


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


instance J.FromJSON Val where
    parseJSON (J.Number v) = let parsedNum = Sci.floatingOrInteger v in
                             return $ case parsedNum of
                                         Left d -> DoubleVal d
                                         Right i -> IntVal i
    parseJSON (J.Bool v) = return $ BoolVal v
    parseJSON (J.String v) = return $ TextVal v
    parseJSON _ = mzero


instance J.FromJSON PropertyValue where
    parseJSON (J.Array v) = ArrayProperty <$> mapM J.parseJSON (V.toList v)
    parseJSON v = ValueProperty <$> J.parseJSON v


type Properties = M.HashMap T.Text PropertyValue

data Node = Node {nodeLocation :: T.Text, nodeProperties :: Properties} deriving (Show)

instance J.FromJSON Node where
    parseJSON (J.Object v) = Node <$> v .: "self" <*> (v .: "data" >>= parseProperties)
        where parseProperties propJson = J.parseJSON propJson


data Relationship = Relationship

newtype Label = Label {runLabel :: T.Text}


-- | An error in handling a Cypher query, either in communicating with the server or parsing the result
data Neo4jException = Neo4jServerException HC.HttpException | 
					   Neo4jClientException String deriving (Show, Typeable)
instance Exception Neo4jException

data Connection = Connection {dbHostname :: Hostname, dbPort :: Port, manager :: HC.Manager}

type Hostname = S.ByteString
type Port = Int


newConnection :: Hostname -> Port -> ResourceT IO Connection
newConnection hostname port = do
        manager <- liftIO $ HC.newManager HC.conduitManagerSettings
        return $ Connection hostname port manager


withConnection :: Hostname -> Port -> Neo4j a -> IO a 
withConnection hostname port cmds = runResourceT $ do
        conn <- newConnection hostname port
        runNeo4j cmds conn
        

newtype Neo4j a = Neo4j { runNeo4j :: Connection -> ResourceT IO a }


instance Monad Neo4j where
    return x = Neo4j (const (return x))
    (Neo4j cmd) >>= f = Neo4j $ \con -> do
                            a <- cmd con
                            runNeo4j (f a) con

instance MonadIO Neo4j where
	liftIO f = Neo4j $ const (liftIO f)



neo4j :: Connection -> Neo4j a -> ResourceT IO a
neo4j con n = runNeo4j n con


httpReq :: Connection -> HT.Method -> S.ByteString -> L.ByteString -> (HT.Status -> Bool) ->
     ResourceT IO (HC.Response L.ByteString)
httpReq (Connection h p m) method path body statusCheck = do
            let request = def {
                    HC.host = h,
                    HC.port = p,
                    HC.path = path,
                    HC.method = method,
                    HC.requestBody = HC.RequestBodyLBS body,
                    HC.checkStatus = \s r c -> if statusCheck s
                                                 then Nothing
                                                 else Just (toException $ HC.StatusCodeException s r c),
                    HC.requestHeaders = [(HT.hAccept, "application/json; charset=UTF-8"),
                                          (HT.hContentType, "application/json")]}
            -- TODO: Would be better to use exceptions package Control.Monad.Catch ??
            -- Wrapping up HTTP-Conduit exceptions in our own
            liftIO (HC.httpLbs request m `catch` \e -> throw $ Neo4jServerException e)


-- | Launch a POST, this will fail if no location header could be retrieved or 201 is not received
httpCreate :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> ResourceT IO a
httpCreate conn path body = do
            res <- httpReq conn HT.methodPost path body (== HT.status201)
            let body = J.eitherDecode $ HC.responseBody res
            return $ case body of
                        Right entity -> entity
                        Left e -> throw $ Neo4jClientException ("Error parsing created entity: " ++ e)


httpRetrieve :: J.FromJSON a => Connection -> S.ByteString -> ResourceT IO (Maybe a)
httpRetrieve conn path = do
            res <- httpReq conn HT.methodGet path "" (\s -> s == HT.status200 || s == HT.status404)
            let status = HC.responseStatus res
            let body = if status == HT.status200
                         then Just $ J.eitherDecode $ HC.responseBody res
                         else Nothing
            return $ case body of
                        Just (Right entity) -> Just entity
                        Just (Left e) -> throw $ Neo4jClientException ("Error parsing created entity: " ++ e)
                        Nothing -> Nothing


nodePath :: S.ByteString
nodePath = "/db/data/node"

createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn ->  do
            httpCreate conn nodePath (J.encode props)


getNode :: S.ByteString -> Neo4j (Maybe Node)
getNode idNode = Neo4j $ \conn -> do
            httpRetrieve conn (nodePath <> "/" <> idNode)


test = do
        withConnection "localhost" 7474 $ do
                node <- createNode $ M.fromList [("hola", ValueProperty $ IntVal 1), 
                                                    ("uuu", ArrayProperty [DoubleVal 2.4, DoubleVal 0.99]),
                                                ("adeu", ValueProperty $ TextVal "hol")]
                let nodeId = S.drop (S.length $ nodePath <> "/") (TE.encodeUtf8 $ nodeLocation node)
                newNode <- getNode nodeId
                otherNewNode <- getNode "4"
                liftIO $ putStrLn $ show node
                liftIO $ putStrLn $ show nodeId 
                liftIO $ putStrLn $ show newNode
                liftIO $ putStrLn $ show otherNewNode
                return node
