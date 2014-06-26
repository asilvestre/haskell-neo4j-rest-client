{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Http where

import Control.Exception.Base (Exception, throw, catch, toException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Default (def)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types


-- | Create a new connection that can be manually closed with runResourceT
newConnection :: Hostname -> Port -> ResourceT IO Connection
newConnection hostname port = do
        mgr <- liftIO $ HC.newManager HC.conduitManagerSettings
        return $ Connection hostname port mgr

-- | Run a set of Neo4j commands in a single connection
withConnection :: Hostname -> Port -> Neo4j a -> IO a 
withConnection hostname port cmds = runResourceT $ do
        conn <- newConnection hostname port
        runNeo4j cmds conn
        
-- | General function for HTTP requests
httpReq :: Connection -> HT.Method -> S.ByteString -> L.ByteString -> (HT.Status -> Bool) ->
     ResourceT IO (HC.Response L.ByteString)
httpReq (Connection h p m) method path body statusCheck = do
            let request = def {
                    HC.host = h,
                    HC.port = p,
                    HC.path = path,
                    HC.method = method,
                    HC.requestBody = HC.RequestBodyLBS body,
                    HC.checkStatus = \s _ _ -> if statusCheck s
                                                 then Nothing
                                                 else Just (toException $ Neo4jUnexpectedResponseException s),
                    HC.requestHeaders = [(HT.hAccept, "application/json; charset=UTF-8"),
                                          (HT.hContentType, "application/json")]}
            -- TODO: Would be better to use exceptions package Control.Monad.Catch ??
            -- Wrapping up HTTP-Conduit exceptions in our own
            liftIO $ HC.httpLbs request m `catch` wrapException
    where wrapException :: HC.HttpException -> a
          wrapException = throw . Neo4jHttpException . show

-- | Launch a POST request, this will raise an exception if 201 or 204 is not received
httpCreate :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> ResourceT IO a
httpCreate conn path body = do
            res <- httpReq conn HT.methodPost path body (==HT.status201)
            let resBody = J.eitherDecode $ HC.responseBody res
            return $ case resBody of
                        Right entity -> entity
                        Left e -> throw $ Neo4jParseException ("Error parsing created entity: " ++ e)

-- | Launch a POST request that doesn't expect response body, this will raise an exception if 204 is not received
httpCreate_ :: Connection -> S.ByteString -> L.ByteString -> ResourceT IO ()
httpCreate_ conn path body = do
            _ <- httpReq conn HT.methodPost path body (\s -> s == HT.status201 || s == HT.status204)
            return ()

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
                        Just (Left e) -> throw $ Neo4jParseException ("Error parsing received entity: " ++ e)
                        Nothing -> Nothing

-- | Launch GET request, just allow 200, if 404 is received an exception will be raised
httpRetrieveSure :: J.FromJSON a => Connection -> S.ByteString -> ResourceT IO a
httpRetrieveSure conn path = do
            res <- httpReq conn HT.methodGet path "" (==HT.status200)
            let body = J.eitherDecode $ HC.responseBody res
            return $ case body of
                        Right entity -> entity
                        Left e -> throw $ Neo4jParseException ("Error parsing received entity: " ++ e)

-- | Launch a GET request, this will raise an exception if 200 or 404 is not received
--   Unlike httpRetrieve this method can parse any JSON value even if it's non-top (arrays and objects)
httpRetrieveValue :: J.FromJSON a => Connection -> S.ByteString -> ResourceT IO (Maybe a)
httpRetrieveValue conn path = do
            res <- httpReq conn HT.methodGet path "" (\s -> s == HT.status200 || s == HT.status404)
            let status = HC.responseStatus res
            let body = if status == HT.status200
                         -- Ugly hack to parse values that aren't top level JSON values (objects and arrays)
                         then Just $ J.eitherDecode $ "[" `L.append` HC.responseBody res `L.append` "]" 
                         else Nothing
            return $ case body of
                        Just (Right (entity:[])) -> Just entity
                        Just (Right _) -> throw $ Neo4jParseException "Error parsing received entity"
                        Just (Left e) -> throw $ Neo4jParseException ("Error parsing received entity: " ++ e)
                        Nothing -> Nothing


-- | Launch a DELETE request, this will raise an exception if 204 is not received
--   Optionally, if passing acceptConflict as True, 409 is accepted too, receiveing 409 makes the function return False
--   If receive 404, we will just return true, though wasn't existing already the result is the same
httpDelete :: Connection -> S.ByteString -> Bool -> ResourceT IO Bool
httpDelete c pth acceptConflict = do
            res <- httpReq c HT.methodDelete pth "" (\s -> s == HT.status204 || s == HT.status404 ||
                                                     (acceptConflict && s == HT.status409))
            let status = HC.responseStatus res
            return $ status /= HT.status409


-- | Launch a PUT request, this will raise an exception if 200 or 204 is not received
httpModify :: Connection -> S.ByteString -> L.ByteString -> ResourceT IO ()
httpModify c path body = do
            _ <- httpReq c HT.methodPut path body (\s -> s == HT.status200 || s == HT.status204)
            return ()
