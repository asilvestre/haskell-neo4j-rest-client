{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Property where


import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Database.Neo4j.Types
import Database.Neo4j.Http


-- | Retrieve relationship/node properties from the DB, if the entity is not present it will raise an exception
getProperties :: Entity a => a -> Neo4j Properties
getProperties e = Neo4j $ \conn -> httpRetrieveSure conn (propertyPath e)

-- | Get a relationship/node property
getProperty :: Entity a => a -> T.Text -> Neo4j (Maybe PropertyValue)
getProperty e prop =  Neo4j $ \conn -> httpRetrieveValue conn (
            propertyPath e <> "/" <> TE.encodeUtf8 prop)

-- | Set all relationship/node properties
setProperties :: Entity a => a -> Properties -> Neo4j a
setProperties e props =  Neo4j $ \conn -> do
            httpModify conn (propertyPath e) $ J.encode props
            return $ setEntityProperties e props

-- | Set a relationship/node property
setProperty :: Entity a => a -> T.Text -> PropertyValue -> Neo4j a 
setProperty e name value =  Neo4j $ \conn -> do
            httpModify conn (propertyPath e <> "/" <> TE.encodeUtf8 name) $ J.encode value
            return $ setEntityProperties e $ M.insert name value (getEntityProperties e)

-- | Delete all relationship/node properties
deleteProperties :: Entity a => a -> Neo4j a
deleteProperties e = Neo4j $ \conn -> do
            _ <- httpDelete conn (propertyPath e) False
            return $ setEntityProperties e emptyProperties

-- | Delete a relationship/node property
--   TODO: Now any 404 raises exception, maybe it's better to differentiate when the 404 comes from an unexisting
--   property from when it comes from an unexisting entity?
deleteProperty :: Entity a => a -> T.Text -> Neo4j a
deleteProperty e name = Neo4j $ \conn -> do
            _ <- httpDelete conn (propertyPath e <> "/" <> TE.encodeUtf8 name) False
            return $ setEntityProperties e $ M.delete name (getEntityProperties e)
