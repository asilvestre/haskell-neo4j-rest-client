{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Cypher (
    -- * Types
    Response(..), ParamValue(..), Params, newparam,
    -- * Sending queries
    cypher, fromResult, fromSuccess, isSuccess
    ) where

import Data.Aeson ((.=), (.:))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

import Database.Neo4j.Types
import Database.Neo4j.Http

-- | Type for a Cypher response with tuples containing column name and their values
data Response = Response {cols :: [T.Text], vals :: [[J.Value]]} deriving (Show, Eq)

-- | How to create a response object from a cypher JSON response
instance J.FromJSON Response where
    parseJSON (J.Object o) = Response <$> (o .: "columns" >>= J.parseJSON) <*> (o .: "data" >>= J.parseJSON)
    parseJSON _ = mzero
                        
cypherAPI :: S.ByteString
cypherAPI = "/db/data/cypher"

-- | Value for a cypher parmeter value, might be a literal, a property map or a list of property maps
data ParamValue = ParamLiteral PropertyValue | ParamProperties Properties | ParamPropertiesArray [Properties]
     deriving (Show, Eq)

newparam :: PropertyValueConstructor a => a -> ParamValue
newparam = ParamLiteral . newval

-- | Instance toJSON for param values so we can serialize them in queries
instance J.ToJSON ParamValue where
    toJSON (ParamLiteral l) = J.toJSON l
    toJSON (ParamProperties p) = J.toJSON p
    toJSON (ParamPropertiesArray ps) = J.toJSON ps

-- | We use hashmaps to represent Cypher parameters
type Params = M.HashMap T.Text ParamValue

-- | Run a cypher query
cypher :: T.Text -> Params -> Neo4j (Either T.Text Response)
cypher cmd params = Neo4j $ \conn -> httpCreate4XXExplained conn cypherAPI body
    where body = J.encode $ J.object ["query" .= cmd, "params" .= J.toJSON params]

-- | Get the result of the response or a default value
fromResult :: Response -> Either T.Text Response -> Response
fromResult def (Left _) = def
fromResult _ (Right resp) = resp

-- | Get the result of the response or a default value
fromSuccess :: Either T.Text Response -> Response
fromSuccess (Left _) = error "Cypher.fromSuccess but is Error"
fromSuccess (Right resp) = resp

-- | True if the operation succeeded
isSuccess :: Either T.Text Response -> Bool
isSuccess (Left _) = False
isSuccess (Right _) = True

