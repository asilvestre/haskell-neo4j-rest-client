{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Neo4j.Types where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend)
import Data.Typeable (Typeable)
import Control.Exception.Base (Exception)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import GHC.Generics (Generic)

import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

-- | Type for a single value of a Neo4j property
data Val = IntVal Int64 | BoolVal Bool | TextVal T.Text | DoubleVal Double deriving (Show, Eq)

-- | Wrapping type for a Neo4j single property or array of properties
--   Using these types allows type checking for only correct properties
--   that is int, double, string, boolean and single typed arrays of these, also nulls are not allowed
data PropertyValue = ValueProperty Val | ArrayProperty [Val] deriving (Show, Eq)

-- | This class allows easy construction of property value types from literals
class PropertyValueConstructor a where
    newval :: a -> PropertyValue

instance PropertyValueConstructor Int64 where
    newval v = ValueProperty $ IntVal v

instance PropertyValueConstructor Bool where
    newval v = ValueProperty $ BoolVal v

instance PropertyValueConstructor T.Text where
    newval v = ValueProperty $ TextVal v

instance PropertyValueConstructor Double where
    newval v = ValueProperty $ DoubleVal v

instance PropertyValueConstructor [Int64] where
    newval v = ArrayProperty $ map IntVal v

instance PropertyValueConstructor [Bool] where
    newval v = ArrayProperty $ map BoolVal v

instance PropertyValueConstructor [T.Text] where
    newval v = ArrayProperty $ map TextVal v

instance PropertyValueConstructor [Double] where
    newval v = ArrayProperty $ map DoubleVal v

-- | This operator allows easy construction of property value types from literals
(|:) :: PropertyValueConstructor a => T.Text -> a -> (T.Text, PropertyValue)
name |: v = (name, newval v)

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

-- | Shortcut for emtpy properties
emptyProperties :: M.HashMap T.Text PropertyValue
emptyProperties = M.empty

-- | Tries to get the path from a URL, we try our best otherwise return the url as is
urlPath :: T.Text -> T.Text
urlPath url = fromMaybe url $ T.stripPrefix "http://" url >>= return . T.dropWhile (/='/')

-- | Path without the /db/data part, useful for batch paths and such
urlMinPath :: T.Text -> T.Text
urlMinPath url =  fromMaybe url $ T.stripPrefix "/db/data" (urlPath url)


-- | Class for top-level Neo4j entities (nodes and relationships) useful to have generic property management code
class Entity a where
    entityPath :: a -> S.ByteString
    propertyPath :: a -> S.ByteString
    getEntityProperties :: a -> Properties
    setEntityProperties :: a -> Properties -> a
    
-- | Get the path for a node entity without host and port
nodePath :: Node -> NodePath
nodePath = NodePath . urlPath . runNodeUrl . nodeUrl

newtype NodeUrl = NodeUrl {runNodeUrl :: T.Text} deriving (Show, Eq, Generic)
instance Hashable NodeUrl

-- | Representation of a Neo4j node, has a location URI and a set of properties
data Node = Node {nodeUrl :: NodeUrl, nodeProperties :: Properties} deriving (Show, Eq)

-- | Get the properties of a node
getNodeProperties :: Node -> Properties
getNodeProperties = nodeProperties

-- | JSON to Node
instance J.FromJSON Node where
    parseJSON (J.Object v) = Node <$> (NodeUrl <$> (v .: "self")) <*> (v .: "data" >>= J.parseJSON)
    parseJSON _ = mzero

instance Entity Node where
    entityPath = runNodeIdentifier
    propertyPath n = runNodeIdentifier n <> "/properties"
    getEntityProperties = nodeProperties
    setEntityProperties n props = n {nodeProperties = props}

nodeAPI :: S.ByteString
nodeAPI = "/db/data/node"

newtype NodePath = NodePath {runNodePath :: T.Text} deriving (Show, Eq, Generic)
instance Hashable NodePath

class NodeIdentifier a where
    getNodePath :: a -> NodePath

instance NodeIdentifier Node where
    getNodePath = nodePath

instance NodeIdentifier S.ByteString where
    getNodePath t = NodePath $ TE.decodeUtf8 $ nodeAPI <> "/" <> t

instance NodeIdentifier NodePath where
    getNodePath = id

instance NodeIdentifier NodeUrl where
    getNodePath n = NodePath $ (urlPath . runNodeUrl) n

runNodeIdentifier :: NodeIdentifier a => a -> S.ByteString
runNodeIdentifier = TE.encodeUtf8 . runNodePath . getNodePath

-- | Type for a relationship type description
type RelationshipType = T.Text

-- | Relationship direction
data Direction = Outgoing | Incoming | Any

-- | Get the path for a node entity without host and port
relPath :: Relationship -> RelPath
relPath = RelPath . urlPath . runRelUrl . relUrl

-- | Type for a relationship location
newtype RelUrl = RelUrl {runRelUrl :: T.Text} deriving (Show, Eq, Generic)
instance Hashable RelUrl

-- | Type for a Neo4j relationship, has a location URI, a relationship type, a starting node and a destination node
data Relationship = Relationship {relUrl :: RelUrl,
                                  relType :: RelationshipType,
                                  relProperties :: Properties,
                                  relFrom :: NodeUrl,
                                  relTo :: NodeUrl} deriving (Show, Eq)

-- | Get the properties of a relationship
getRelProperties :: Relationship -> Properties
getRelProperties = relProperties

-- | Get the type of a relationship
getRelType :: Relationship -> RelationshipType
getRelType = relType

-- | JSON to Relationship
instance J.FromJSON Relationship where
    parseJSON (J.Object v) = Relationship <$> (RelUrl <$> v .: "self") <*> v .: "type" <*>
                                (v .: "data" >>= J.parseJSON) <*> (NodeUrl <$> v .: "start") <*>
                                (NodeUrl <$> v .: "end")
    parseJSON _ = mzero

instance Entity Relationship where
    entityPath = runRelIdentifier
    propertyPath r = runRelIdentifier r <> "/properties"
    getEntityProperties = relProperties
    setEntityProperties r props = r {relProperties = props}

relationshipAPI :: S.ByteString
relationshipAPI = "/db/data/relationship"

newtype RelPath = RelPath {runRelPath :: T.Text} deriving (Show, Eq, Generic)
instance Hashable RelPath

class RelIdentifier a where
    getRelPath :: a -> RelPath

instance RelIdentifier Relationship where
    getRelPath = relPath

instance RelIdentifier RelPath where
    getRelPath = id

instance RelIdentifier S.ByteString where
    getRelPath t = RelPath $ TE.decodeUtf8 $ relationshipAPI <> "/" <> t

instance RelIdentifier RelUrl where
    getRelPath = RelPath . urlPath . runRelUrl

runRelIdentifier :: RelIdentifier a => a -> S.ByteString
runRelIdentifier = TE.encodeUtf8 . runRelPath . getRelPath

-- | Type for a label
type Label = T.Text

-- | Type for an index
data Index = Index {indexLabel :: Label, indexProperties :: [T.Text]} deriving (Show, Eq)

-- | JSON to Index
instance J.FromJSON Index where
    parseJSON (J.Object v) = Index <$> v .: "label" <*> (v .: "property_keys" >>= J.parseJSON)
    parseJSON _ = mzero

-- | Exceptions this library can raise
data Neo4jException = Neo4jHttpException String |
                      Neo4jNonOrphanNodeDeletionException S.ByteString |
                      Neo4jNoEntityException S.ByteString |
                      Neo4jUnexpectedResponseException HT.Status |
                      Neo4jParseException String deriving (Show, Typeable, Eq)
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
