{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch where

import Control.Exception.Base (Exception, throw, catch, toException)
import Data.Maybe (fromMaybe)
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (state, State)

import qualified Data.Aeson as J
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Graph
import Database.Neo4j.Types

urlMinPath :: T.Text -> S.ByteString
urlMinPath url =  TE.encodeUtf8 $ fromMaybe url $ T.stripPrefix "/db/data" (urlTextPath url)

newtype BatchFuture a = BatchFuture Int

data BatchCmd = BatchCmd {cmdMethod :: HT.Method, cmdPath :: S.ByteString, cmdBody :: L.ByteString,
                            cmdParse :: L.ByteString -> Graph -> Graph}

type FutureMapping = [(Int, Int)]

data BatchState = BatchState {commands :: [BatchCmd], cmdId :: Int}

newtype Batch a = Batch {runBatch :: State BatchState (BatchFuture a)}

class NodeBatchIdentifier a where
    getNodeBatchId :: a -> S.ByteString

instance NodeBatchIdentifier Node where
    getNodeBatchId = urlMinPath . runNodeLocation . nodeLocation

instance NodeBatchIdentifier (BatchFuture Node) where
    getNodeBatchId (BatchFuture bId) = "/node/{" <>  (C8.pack $ show bId) <> "}"

tryDecode :: J.FromJSON a => L.ByteString -> a
tryDecode jb = let res = J.eitherDecode jb
               in case res of
                     Right entity -> entity
                     Left e -> throw $ Neo4jParseException ("Error parsing entity: " ++ e)

createNode :: Properties -> Batch Node
createNode props = Batch $ state $ \(BatchState cmds cId) -> (BatchFuture $ cId + 1, BatchState (cmd : cmds) (cId + 1))
    where cmd = BatchCmd {cmdMethod = HT.methodPost, cmdPath = "/node", cmdBody = J.encode props, cmdParse = parser}
          parser jsonBody g = addNode (tryDecode jsonBody) g

getNode :: NodeBatchIdentifier a => a -> Batch Node


{--
instance Monad Batch where
    return x = Batch $ const (x, BatchState {commands = [], cmdId = 0})
    (Batch h) >>= f = \s -> let (a, newState) = h s
                                (BatchState g) = f a
                            in g newState

instance MonadIO Neo4j where
	liftIO f = Neo4j $ const (liftIO f)

runBatch :: Batch a -> Neo4j Graph--}
