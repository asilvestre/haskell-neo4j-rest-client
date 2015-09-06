{-# LANGUAGE OverloadedStrings  #-}

module Database.Neo4j.Http where

import Control.Exception.Base (Exception, throw, catch, toException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (def)
import Data.Maybe (fromMaybe)

import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)

import Network.HTTP.Client (defaultManagerSettings)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Types

-- | Create a new connection that can be manually closed with runResourceT
newConnection :: Hostname -> Port -> IO Connection
newConnection hostname port = do
        mgr <- HC.newManager defaultManagerSettings
        return $ Connection hostname port mgr Nothing

-- | Create a new connection that can be manually closed with runResourceT using provided credentials for basic auth
newAuthConnection :: Hostname -> Port -> Credentials -> IO Connection
newAuthConnection hostname port creds = do
        mgr <- HC.newManager defaultManagerSettings
        return $ Connection hostname port mgr (Just creds)

-- | Run a set of Neo4j commands in a single connection
withConnection :: Hostname -> Port -> Neo4j a -> IO a
withConnection hostname port cmds = runResourceT $ do
         mgr <- liftIO $ HC.newManager defaultManagerSettings
         let conn = Connection hostname port mgr Nothing
         liftIO $ runNeo4j cmds conn

-- | Run a set of Neo4j commands in a single connection using provided credentials for basic auth
withAuthConnection :: Hostname -> Port -> Credentials -> Neo4j a -> IO a
withAuthConnection hostname port creds cmds = runResourceT $ do
         mgr <- liftIO $ HC.newManager defaultManagerSettings
         let conn = Connection hostname port mgr (Just creds)
         liftIO $ runNeo4j cmds conn
       
-- | General function for HTTP requests
httpReq :: Connection -> HT.Method -> S.ByteString -> L.ByteString -> (HT.Status -> Bool) ->
     IO (HC.Response L.ByteString)
-- No credentials provided
httpReq (Connection h p m c) method path body statusCheck = do
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
            liftIO $ case c of 
                Nothing -> HC.httpLbs request m `catch` wrapException
                Just creds -> HC.httpLbs (HC.applyBasicAuth (getUsername creds) (getPassword creds) request) m `catch` wrapException
    where wrapException :: HC.HttpException -> a
          wrapException = throw . Neo4jHttpException . show
          
--Credentials provided
{-
httpReq (Connection h p m (Just c)) method path body statusCheck = do
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
            liftIO $ HC.httpLbs (HC.applyBasicAuth (fst c) (snd c) request) m `catch` wrapException
    where wrapException :: HC.HttpException -> a
          wrapException = throw . Neo4jHttpException . show   
-}       

-- | Extracts the exception description from a HTTP Neo4j response if the status code matches otherwise Nothing
extractException :: HC.Response L.ByteString -> T.Text
extractException resp = fromMaybe "" $ do
                resobj <- J.decode $ HC.responseBody resp
                flip parseMaybe resobj $ \obj -> obj .: "exception"

-- | Launch a POST request, this will raise an exception if 201 or 200 is not received
httpCreate :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> IO a
httpCreate conn path body = do
            res <- httpReq conn HT.methodPost path body (`elem` [HT.status200, HT.status201])
            let resBody = J.eitherDecode $ HC.responseBody res
            return $ case resBody of
                        Right entity -> entity
                        Left e -> throw $ Neo4jParseException ("Error parsing created entity: " ++ e)

-- | Launch a POST request and get some headers
httpCreateWithHeaders :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> IO (a, HT.ResponseHeaders)
httpCreateWithHeaders conn path body = do
            res <- httpReq conn HT.methodPost path body (`elem` [HT.status200, HT.status201])
            let resBody = J.eitherDecode $ HC.responseBody res
            let result = case resBody of
                            Right entity -> entity
                            Left e -> throw $ Neo4jParseException ("Error parsing created entity: " ++ e)
            let headers = HC.responseHeaders res
            return (result, headers)

-- | Launch a POST request, this will raise an exception if 201 or 204 is not received, explain 500
httpCreate500Explained :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString ->
                                             IO (Either L.ByteString a)
httpCreate500Explained conn path body = do
            res <- httpReq conn HT.methodPost path body (`elem` [HT.status200, HT.status201, HT.status500])
            let status = HC.responseStatus res
            let resBody = HC.responseBody res
            return $ if status == HT.status500 then Left resBody else parseBody resBody
        where parseBody b = case J.eitherDecode b of
                        Right entity -> Right entity
                        Left e -> throw $ Neo4jParseException ("Error parsing created entity: " ++ e)

-- | Launch a POST request, this will raise an exception if 201 or 204 is not received
--   With 404 or 400 will return a left with the explanation
httpCreate4XXExplained :: J.FromJSON a => Connection -> S.ByteString -> L.ByteString -> IO (Either T.Text a)
httpCreate4XXExplained conn path body = do
            res <- httpReq conn HT.methodPost path body (\s -> s `elem` validcodes ++ errcodes)
            let status = HC.responseStatus res
            return $ if status `elem`  validcodes then parseBody res else Left $ extractException res
    where parseBody resp = case J.eitherDecode $ HC.responseBody resp of
                            Right entity -> Right entity
                            Left e -> throw $ Neo4jParseException ("Error parsing created entity: " ++ e)
          validcodes = [HT.status200, HT.status201]
          errcodes = [HT.status404, HT.status400]

-- | Launch a POST request that doesn't expect response body, this will raise an exception if 204 is not received
httpCreate_ :: Connection -> S.ByteString -> L.ByteString -> IO ()
httpCreate_ conn path body = do
            _ <- httpReq conn HT.methodPost path body (\s -> s == HT.status201 || s == HT.status204)
            return ()

-- | Launch a GET request, this will raise an exception if 200 or 404 is not received
httpRetrieve :: J.FromJSON a => Connection -> S.ByteString -> IO (Maybe a)
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
httpRetrieveSure :: J.FromJSON a => Connection -> S.ByteString -> IO a
httpRetrieveSure conn path = do
            res <- httpReq conn HT.methodGet path "" (==HT.status200)
            let body = J.eitherDecode $ HC.responseBody res
            return $ case body of
                        Right entity -> entity
                        Left e -> throw $ Neo4jParseException ("Error parsing received entity: " ++ e)

-- | Launch a GET request, this will raise an exception if 200 or 404 is not received
--   With 404 will return a left with the explanation
--   Unlike httpRetrieve this method can parse any JSON value even if it's non-top (arrays and objects)
httpRetrieveValue :: J.FromJSON a => Connection -> S.ByteString -> IO (Either T.Text a)
httpRetrieveValue conn path = do
            res <- httpReq conn HT.methodGet path "" (\s -> s == HT.status200 || s == HT.status404)
            let status = HC.responseStatus res
            return $ if status == HT.status200 then parseBody res else Left $ extractException res
                         -- Ugly hack to parse values that aren't top level JSON values (objects and arrays)
    where parseBody resp = case  J.eitherDecode $ "[" `L.append` HC.responseBody resp `L.append` "]" of
                                Right (entity:[]) -> Right entity
                                Right _ -> throw $ Neo4jParseException "Error parsing received entity"
                                Left e -> throw $ Neo4jParseException ("Error parsing received entity: " ++ e)
            

-- | Launch a DELETE request, this will raise an exception if 204 is not received
--   If we receive 404, we will just return true, though wasn't existing already the result is the same
httpDelete :: Connection -> S.ByteString -> IO () 
httpDelete c pth = do
            _ <- httpReq c HT.methodDelete pth "" (\s -> s == HT.status204 || s == HT.status404)
            return ()

-- | Launch a DELETE request, this will raise an exception if 204 is not received
--   We don't accept 404
httpDeleteNo404 :: Connection -> S.ByteString -> IO () 
httpDeleteNo404 c pth = do
            _ <- httpReq c HT.methodDelete pth "" (==HT.status204)
            return ()

-- | Launch a DELETE request, this will raise an exception if 204 is not received
--   If we receive 404, we will return the server explanation
httpDelete404Explained :: Connection -> S.ByteString -> IO (Either T.Text ())
httpDelete404Explained c pth = do
            res <- httpReq c HT.methodDelete pth "" (\s -> s == HT.status204 || s == HT.status404)
            let status = HC.responseStatus res
            return $ if status /= HT.status404 then Right () else Left $ extractException res

-- | Launch a PUT request, this will raise an exception if 200 or 204 is not received
httpModify :: Connection -> S.ByteString -> L.ByteString -> IO ()
httpModify c path body = do
            _ <- httpReq c HT.methodPut path body (\s -> s == HT.status200 || s == HT.status204)
            return ()

-- | Launch a PUT request, this will raise an exception if 200 or 204 is not received
--   If we receive 404, we will return the server explanation
httpModify404Explained :: Connection -> S.ByteString -> L.ByteString -> IO (Either T.Text ())
httpModify404Explained c path body = do
            res <- httpReq c HT.methodPut path body (\s -> s == HT.status200 || s == HT.status204 || s == HT.status404)
            let status = HC.responseStatus res
            return $ if status /= HT.status404 then Right () else Left $ extractException res

-- | Wrap 404 exception into Neo4jNoEntity exceptions
proc404Exc :: Entity e => e -> Neo4jException -> a
proc404Exc e exc@(Neo4jUnexpectedResponseException s)
        | s == HT.status404 = throw (Neo4jNoEntityException $ entityPath e)
        | otherwise = throw exc
proc404Exc _ exc = throw exc

