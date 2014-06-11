{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Neo4j.Types where

import Data.Monoid (mappend)
import Data.Typeable (Typeable)
import Control.Exception.Base (Exception)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit as HC

(<>) :: S.ByteString -> S.ByteString -> S.ByteString
(<>) = mappend

-- | Type for a single value of a Neo4j property
data Val = IntVal Int64 | BoolVal Bool | TextVal T.Text | DoubleVal Double deriving (Show)

-- | Wrapping type for a Neo4j single property or array of properties
--   Using these types allows type checking for only correct properties
--   that is int, double, string, boolean and single typed arrays of these, also nulls are not allowed
data PropertyValue = ValueProperty Val | ArrayProperty [Val] deriving (Show)

-- | This class allows easy construction of property value types from literals
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

-- | This operator allows easy construction of property value types from literals
(|:) :: PropertyValueConstructor a => T.Text -> a -> (T.Text, PropertyValue)
name |: v = (name, propvalueconstr v)

-- | Specifying how to convert property single values to JSON
instance J.ToJSON Val where
    toJSON (IntVal v)  = J.Number $ fromIntegral v
    toJSON (BoolVal v)  = J.Bool v
    toJSON (TextVal v)  = J.String v
    toJSON (DoubleVal v)  = J.Number $ Sci.fromFloatDigits v

-- | Specifying how to convert property values to JSON
instance J.ToJSON PropertyValue where
    toJSON (ValueProperty v) = J.toJSON v
    toJSON (ArrayProperty vs) = J.Array (V.fromList $ map J.toJSON vs) 

-- | JSON to single property values
instance J.FromJSON Val where
    parseJSON (J.Number v) = let parsedNum = Sci.floatingOrInteger v in
                             return $ case parsedNum of
                                         Left d -> DoubleVal d
                                         Right i -> IntVal i
    parseJSON (J.Bool v) = return $ BoolVal v
    parseJSON (J.String v) = return $ TextVal v
    parseJSON _ = mzero

-- | JSON to property values
instance J.FromJSON PropertyValue where
    parseJSON (J.Array v) = ArrayProperty <$> mapM J.parseJSON (V.toList v)
    parseJSON v = ValueProperty <$> J.parseJSON v

-- | We use hashmaps to represent Neo4j properties
type Properties = M.HashMap T.Text PropertyValue

-- | Tries to get the path from a URL, we try our best otherwise return the url as is
urlPath :: T.Text -> S.ByteString
urlPath url = TE.encodeUtf8 $ fromMaybe url $ T.stripPrefix "http://" url >>= return . T.dropWhile (/='/')
    
-- | Get the path for a node entity without host and port
nodePath :: Node -> S.ByteString
nodePath = urlPath . nodeLocation

-- | Representation of a Neo4j node, has a location URI and a set of properties
data Node = Node {nodeLocation :: T.Text, nodeProperties :: Properties} deriving (Show)

-- | JSON to Node
instance J.FromJSON Node where
    parseJSON (J.Object v) = Node <$> (v .: "self") <*> (v .: "data" >>= J.parseJSON)
    parseJSON _ = mzero

-- | Type for a relationship type description
type RelationshipType = T.Text

-- | Get the path for a node entity without host and port
relPath :: Relationship -> S.ByteString
relPath = urlPath . relLocation

-- | Type for a Neo4j relationship, has a location URI, a relationship type, a starting node and a destination node
data Relationship = Relationship {relLocation :: T.Text,
                                  relType :: RelationshipType,
                                  relProperties :: Properties,
                                  relFrom :: Node,
                                  relTo :: Node} deriving (Show)

-- | JSON to Relationship
instance J.FromJSON Relationship where
    parseJSON (J.Object v) = Relationship <$> v .: "self" <*> v .: "type" <*> (v .: "data" >>= J.parseJSON) <*>
                                v .: "start" <*> v .: "end"
    parseJSON _ = mzero

-- | Type for a label
newtype Label = Label {runLabel :: T.Text}

-- | Exceptions this library can raise
data Neo4jException = Neo4jServerException HC.HttpException | Neo4jClientException String deriving (Show, Typeable)
instance Exception Neo4jException

-- | Type for a connection
data Connection = Connection {dbHostname :: Hostname, dbPort :: Port, manager :: HC.Manager}

type Hostname = S.ByteString
type Port = Int

-- | Neo4j monadic type to be able to sequence neo4j commands in a connection
newtype Neo4j a = Neo4j { runNeo4j :: Connection -> ResourceT IO a }

instance Monad Neo4j where
    return x = Neo4j (const (return x))
    (Neo4j cmd) >>= f = Neo4j $ \con -> do
                            a <- cmd con
                            runNeo4j (f a) con

instance MonadIO Neo4j where
	liftIO f = Neo4j $ const (liftIO f)
