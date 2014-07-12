{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch where

import Control.Exception.Base (throw)
import Data.Aeson ((.=))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Control.Monad.State (state, State, runState)

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Graph
import Database.Neo4j.Http
import Database.Neo4j.Types

urlMinPath :: T.Text -> T.Text
urlMinPath url =  fromMaybe url $ T.stripPrefix "/db/data" (urlTextPath url)

newtype BatchFuture a = BatchFuture Int

type CmdParser = J.Value -> Graph -> Graph

data BatchCmd = BatchCmd {cmdMethod :: HT.Method, cmdPath ::T.Text, cmdBody :: J.Value,
                            cmdParse :: CmdParser, cmdId :: Int}

defCmd :: BatchCmd
defCmd = BatchCmd{cmdMethod = HT.methodGet, cmdPath = "", cmdBody = "", cmdParse = \_ g -> g, cmdId = -1}

data BatchState = BatchState {commands :: [BatchCmd], batchId :: Int}

newtype Batch a = Batch (State BatchState (BatchFuture a))

getCmds :: Batch a -> [BatchCmd]
getCmds (Batch s) = let (_, BatchState cmds _) = runState s (BatchState [] (-1)) in cmds

instance J.ToJSON (Batch a) where
    toJSON b = J.toJSON $ getCmds b

instance J.ToJSON BatchCmd where
    toJSON (BatchCmd m p b _ cId) = J.object ["method" .= S.unpack m, "to" .= p, "body" .= b, "id" .= cId]

class NodeBatchIdentifier a where
    getNodeBatchId :: a -> T.Text

instance NodeBatchIdentifier Node where
    getNodeBatchId = urlMinPath . runNodeLocation . nodeLocation

instance NodeBatchIdentifier (BatchFuture Node) where
    getNodeBatchId (BatchFuture bId) = "/node/{" <> (fromString . show) bId <> "}"

tryParse :: J.FromJSON a => J.Value -> a
tryParse jb = let res = flip JT.parseEither jb $ \obj -> J.parseJSON obj
               in case res of
                     Right entity -> entity
                     Left e -> throw $ Neo4jParseException ("Error parsing entity: " ++ e)

nextState :: BatchCmd -> Batch a
nextState cmd = Batch $ state $ \(BatchState cmds cId) ->
                                     (BatchFuture $ cId + 1, BatchState (cmd{cmdId = cId + 1} : cmds) (cId + 1))

createNode :: Properties -> Batch Node
createNode props = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodPost, cmdPath = "/node", cmdBody = J.toJSON props, cmdParse = parser}
          parser n = addNode (tryParse n)

getNode :: NodeBatchIdentifier a => a -> Batch Node
getNode n = nextState cmd
    where cmd = defCmd{cmdMethod = HT.methodGet, cmdPath = getNodeBatchId n, cmdBody = "", cmdParse = parser}
          parser jn = addNode (tryParse jn)

parseBatchResponse :: J.Array -> Batch a -> Graph
parseBatchResponse jarr b = foldr ($) empty appliedParsers
    where cmds = getCmds b
          parsers = foldl (\ps cmd -> cmdParse cmd : ps) [] cmds
          appliedParsers = zipWith ($) parsers (V.toList jarr)

runBatch :: Batch a -> Neo4j Graph
runBatch b = Neo4j $ \conn -> do
        arr <- httpCreate conn "/db/data/batch" $ J.encode b
        return $ parseBatchResponse arr b
