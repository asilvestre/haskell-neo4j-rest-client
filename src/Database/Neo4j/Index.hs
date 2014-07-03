{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Index where

import Data.Aeson ((.=))

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http


indexPath :: Label -> Maybe T.Text -> S.ByteString
indexPath lbl prop = "/db/data/schema/index/" <> TE.encodeUtf8 lbl <> propPath prop
    where propPath (Just name) = "/" <> TE.encodeUtf8 name
          propPath Nothing = ""

-- | Creates an index for a label and a property
createIndex :: Label -> T.Text -> Neo4j Index
createIndex lbl prop = Neo4j $ \conn -> httpCreate conn (indexPath lbl Nothing) body
    where body = J.encode $ J.object ["property_keys" .= [prop]]

-- | Gets all indexes for a label
getIndexes :: Label -> Neo4j [Index]
getIndexes lbl = Neo4j $ \conn -> httpRetrieveSure conn (indexPath lbl Nothing)

-- | Drop and index
dropIndex :: Label -> T.Text -> Neo4j ()
dropIndex lbl prop = Neo4j $ \conn -> httpDelete conn (indexPath lbl (Just prop))
