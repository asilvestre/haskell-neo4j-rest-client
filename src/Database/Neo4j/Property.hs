{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Property where

import Control.Exception.Lifted (throw, catch)

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types
import Database.Neo4j.Http


-- | Wrap 404 exception into Neo4jNoEntity exceptions
proc404Exc :: Entity e => e -> Neo4jException -> a
proc404Exc e exc@(Neo4jUnexpectedResponseException s)
        | s == HT.status404 = throw (Neo4jNoEntityException $ entityPath e)
        | otherwise = throw exc
proc404Exc _ exc = throw exc

-- | Retrieve relationship/node properties from the DB, if the entity is not present it will raise an exception
--   If the entity doesn't exist it will raise a Neo4jNoEntity exception
getProperties :: Entity a => a -> Neo4j Properties
getProperties e = Neo4j $ \conn -> httpRetrieveSure conn (propertyPath e) `catch` proc404Exc e

-- | Get a relationship/node property
--   If the 404 is because the parent entity doesn't exist we'll raise the corresponding Neo4jNoEntity
--   If the 404 is because there is no property just return Nothing
getProperty :: Entity a => a -> T.Text -> Neo4j (Maybe PropertyValue)
getProperty e prop =  Neo4j $ \conn -> do
        res <- httpRetrieveValue conn path
        case res of
                    Right value -> return value
                    Left expl -> if expl `elem` excList then throw (Neo4jNoEntityException $ entityPath e)
                                 else return Nothing
    where path = propertyPath e <> "/" <> TE.encodeUtf8 prop
          excList = ["RelationshipNotFoundException", "NodeNotFoundException"]

-- | Set all relationship/node properties
--   If the entity doesn't exist it will raise a Neo4jNoEntity exception
setProperties :: Entity a => a -> Properties -> Neo4j a
setProperties e props =  Neo4j $ \conn -> (do
            httpModify conn (propertyPath e) $ J.encode props
            return $ setEntityProperties e props) `catch` proc404Exc e

-- | Set a relationship/node property
--   If the entity doesn't exist it will raise a Neo4jNoEntity exception
setProperty :: Entity a => a -> T.Text -> PropertyValue -> Neo4j a 
setProperty e name value =  Neo4j $ \conn -> (do
            httpModify conn (propertyPath e <> "/" <> TE.encodeUtf8 name) $ J.encode value
            return $ setEntityProperties e $ M.insert name value (getEntityProperties e)) `catch` proc404Exc e

-- | Delete all relationship/node properties
--   If the entity doesn't exist it will raise a Neo4jNoEntity exception
deleteProperties :: Entity a => a -> Neo4j a
deleteProperties e = Neo4j $ \conn -> (do
            httpDeleteNo404 conn (propertyPath e)
            return $ setEntityProperties e emptyProperties) `catch` proc404Exc e

-- | Delete a relationship/node property
--   If the entity doesn't exist it will raise a Neo4jNoEntity exception
deleteProperty :: Entity a => a -> T.Text -> Neo4j a
deleteProperty e name = Neo4j $ \conn -> do
            res <- httpDelete404Explained conn (propertyPath e <> "/" <> TE.encodeUtf8 name)
            case res of
                    Right _ -> return resultingProps
                    Left expl -> if expl `elem` excList then throw (Neo4jNoEntityException $ entityPath e)
                                 else return resultingProps
    where excList = ["RelationshipNotFoundException", "NodeNotFoundException"]
          resultingProps = setEntityProperties e $ M.delete name (getEntityProperties e)

