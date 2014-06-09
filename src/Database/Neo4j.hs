{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Data.Aeson ((.:), (.=))
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
    
--iv = ListVal' [False]

data Val = IntVal Int64 | BoolVal Bool | TextVal T.Text | DoubleVal Double deriving (Show)
data PropertyValue = ValueProperty Val | ArrayProperty [Val] deriving (Show)

class PropertyValueConstructor a where
    propvalueconstr :: a -> PropertyValue

instance PropertyValueConstructor Int64 where
    propvalueconstr v = ValueProperty $ IntVal v

instance PropertyValueConstructor Bool where
    propvalueconstr v = ValueProperty $ BoolVal v

instance PropertyValueConstructor T.Text where
    propvalueconstr v = ValueProperty $ TextVal v

instance PropertyValueConstructor Double where
    propvalueconstr v = ValueProperty $ DoubleVal v

instance PropertyValueConstructor [Int64] where
    propvalueconstr v = ArrayProperty $ map IntVal v

instance PropertyValueConstructor [Bool] where
    propvalueconstr v = ArrayProperty $ map BoolVal v

instance PropertyValueConstructor [T.Text] where
    propvalueconstr v = ArrayProperty $ map TextVal v

instance PropertyValueConstructor [Double] where
    propvalueconstr v = ArrayProperty $ map DoubleVal v

(|:) :: PropertyValueConstructor a => T.Text -> a -> (T.Text, PropertyValue)
name |: v = (name, propvalueconstr v)


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

nodePath = TE.encodeUtf8 . nodeLocation

instance J.FromJSON Node where
    parseJSON (J.Object v) = Node <$> v .: "self" <*> (v .: "data" >>= parseProperties)
        where parseProperties propJson = J.parseJSON propJson
    parseJSON _ = mzero

type RelationshipType = T.Text

data Relationship = Relationship {relLocation :: T.Text,
                                  relType :: RelationshipType,
                                  relProperties :: Properties,
                                  relFrom :: Node,
                                  relTo :: Node} deriving (Show)

instance J.FromJSON Relationship where
    parseJSON (J.Object v) = Relationship <$> v .: "self" <*> v .: "type" <*> (v .: "data" >>= parseProperties) <*>
                                v .: "start" <*> v .: "end"
        where parseProperties propJson = J.parseJSON propJson


relPath = TE.encodeUtf8 . relLocation

newtype Label = Label {runLabel :: T.Text}


data Neo4jException = Neo4jServerException HC.HttpException | Neo4jClientException String deriving (Show, Typeable)
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


-- | Launch a POST request, this will raise an exception if 201 is not received
httpCreate :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> ResourceT IO a
httpCreate conn path body = do
            res <- httpReq conn HT.methodPost path body (== HT.status201)
            let body = J.eitherDecode $ HC.responseBody res
            return $ case body of
                        Right entity -> entity
                        Left e -> throw $ Neo4jClientException ("Error parsing created entity: " ++ e)


-- | Launch a GET request, this will raise an exception if 200 or 404 is not received
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


-- | Launch a DELETE request, this will raise an exception if 204 is not received
--   Optionally, if passing acceptConflict as True, 409 is accepted too, receiveing 409 makes the function return False
httpDelete :: Connection -> S.ByteString -> Bool -> ResourceT IO Bool
httpDelete c pth acceptConflict = do
            res <- httpReq c HT.methodDelete pth "" (\s -> s == HT.status204 || (acceptConflict && s == HT.status409))
            let status = HC.responseStatus res
            return $ status == HT.status204


nodeAPI :: S.ByteString
nodeAPI = "/db/data/node"

-- | Create a new node with a set of properties
createNode :: Properties -> Neo4j Node
createNode props = Neo4j $ \conn ->  do
            httpCreate conn nodeAPI (J.encode props)

-- | Get a node by ID
getNodeById :: S.ByteString -> Neo4j (Maybe Node)
getNodeById idNode = Neo4j $ \conn -> do
            httpRetrieve conn (nodeAPI <> "/" <> idNode)

-- | Refresh a node entity with the contents in the DB
getNode :: Node -> Neo4j (Maybe Node)
getNode n = Neo4j $ \conn -> do
            httpRetrieve conn (nodePath n)

-- | Delete a node by ID, the deletion will fail if the node is not orphan, if that happens the result will be False
deleteNodeById :: S.ByteString -> Neo4j Bool
deleteNodeById idNode = Neo4j $ \conn -> do
            httpDelete conn (nodeAPI <> "/" <> idNode) True

-- | Delete a node, the deletion will fail if the node is not orphan, if that happens the result will be False
deleteNode :: Node -> Neo4j Bool
deleteNode n = Neo4j $ \conn -> do
            httpDelete conn (nodePath n) True

relationshipAPI :: S.ByteString
relationshipAPI = "/db/data/relationship"

-- | Create a new relationship with a type and a set of properties
createRelationship :: RelationshipType -> Properties -> Node -> Node -> Neo4j Relationship
createRelationship t props nodefrom nodeto = Neo4j $ \conn -> do
            httpCreate conn reqPath reqBody
    where reqPath = nodePath nodefrom <> "/relationships"
          reqBody = J.encode $ J.object ["to" .= nodeLocation nodeto, "type" .= t, "data" .= J.toJSON props]

-- | Get relationship by ID
getRelationshipById :: S.ByteString -> Neo4j (Maybe Relationship)
getRelationshipById idRel = Neo4j $ \conn -> do
            httpRetrieve conn (relationshipAPI <> "/" <> idRel)

-- | Refresh a relationship entity with the contents in the DB
getRelationship :: Relationship -> Neo4j (Maybe Relationship)
getRelationship rel = Neo4j $ \conn -> do
            httpRetrieve conn (relPath rel)

-- | Delete a relationship by ID
deleteRelationshipById :: S.ByteString -> Neo4j ()
deleteRelationshipById idRel = Neo4j $ \conn -> do
            _ <- httpDelete conn (relationshipAPI <> "/" <> idRel) False
            return ()

-- | Delete a relationship
deleteRelationship :: Relationship -> Neo4j ()
deleteRelationship rel = Neo4j $ \conn -> do
            _ <- httpDelete conn (relPath rel) False
            return ()

-- | Get all properties of a relationship from the DB
getRelationShipProperties :: Relationship -> Neo4j Properties
getRelationShipProperties rel = Neo4j $ \conn -> do
            _ <- httpDelete conn (relPath rel) False
            return ()


test = do
        withConnection "localhost" 7474 $ do
                nodev <- createNode $ M.fromList ["hola" |: ("hhh" :: T.Text)]--[False, True]]
                node <- createNode $ M.fromList [("hola", ValueProperty $ IntVal 1), 
                                                    ("uuu", ArrayProperty [DoubleVal 2.4, DoubleVal 0.99]),
                                                ("adeu", ValueProperty $ TextVal "hol")]
                let nodeId = S.drop (S.length $ nodeAPI <> "/") (TE.encodeUtf8 $ nodeLocation node)
                newNode <- getNodeById nodeId
                otherNewNode <- getNodeById "4"
                liftIO $ putStrLn $ show node
                liftIO $ putStrLn $ show nodeId 
                liftIO $ putStrLn $ show newNode
                liftIO $ putStrLn $ show otherNewNode
                return node
