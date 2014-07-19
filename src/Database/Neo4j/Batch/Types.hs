{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch.Types where

import Control.Exception.Base (throw)
import Data.Aeson ((.=), (.:))
import Data.Maybe (fromMaybe)
import Control.Monad.State (state, State, runState)

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Types as HT

import Database.Neo4j.Graph
import Database.Neo4j.Http
import Database.Neo4j.Types

newtype BatchFuture a = BatchFuture Int

type CmdParser = J.Value -> Graph -> Graph

data BatchCmd = BatchCmd {cmdMethod :: HT.Method, cmdPath ::T.Text, cmdBody :: J.Value,
                            cmdParse :: CmdParser, cmdId :: Int}

defCmd :: BatchCmd
defCmd = BatchCmd{cmdMethod = HT.methodGet, cmdPath = "", cmdBody = "", cmdParse = \_ g -> g, cmdId = -1}

data BatchState = BatchState {commands :: [BatchCmd], batchId :: Int}

type Batch a = State BatchState a

getCmds :: Batch a -> [BatchCmd]
getCmds s = let (_, BatchState cmds _) = runState s (BatchState [] (-1)) in reverse cmds

instance J.ToJSON (Batch a) where
    toJSON b = J.toJSON $ getCmds b

instance J.ToJSON BatchCmd where
    toJSON (BatchCmd m p b _ cId) = J.object ["method" .= TE.decodeUtf8 m, "to" .= p, "body" .= b, "id" .= cId]

-- | Helper function to parse a batch element response from a body entry
tryParseBody :: J.FromJSON a => J.Value -> a
tryParseBody (J.Object jb) = let res = flip JT.parseEither jb $ \obj -> (obj .: "body") >>= J.parseJSON
               in case res of
                     Right entity -> entity
                     Left e -> throw $ Neo4jParseException ("Error parsing entity: " ++ e)
tryParseBody _ = throw $ Neo4jParseException "Error expecting an object"

-- | Helper function to parse a batch element response from a from entry
tryParseFrom :: J.FromJSON a => J.Value -> a
tryParseFrom (J.Object jb) = let res = flip JT.parseEither jb $ \obj -> (obj .: "from") >>= J.parseJSON
               in case res of
                     Right entity -> entity
                     Left e -> throw $ Neo4jParseException ("Error parsing entity: " ++ e)
tryParseFrom _ = throw $ Neo4jParseException "Error expecting an object"

nextState :: BatchCmd -> Batch (BatchFuture a)
nextState cmd = state $ \(BatchState cmds cId) ->
                                 (BatchFuture $ cId + 1, BatchState (cmd{cmdId = cId + 1} : cmds) (cId + 1))

parseBatchResponse :: J.Array -> Batch a -> Graph
parseBatchResponse jarr b = foldl (flip ($)) empty appliedParsers
    where cmds = getCmds b
          parsers = foldr (\cmd ps -> cmdParse cmd : ps) [] cmds
          appliedParsers = zipWith ($) parsers (V.toList jarr)

-- | Get teh exception type for a given batch exception message, if nothing is found a default exception is given
exceptionByName :: Neo4jException -> T.Text -> T.Text -> Neo4jException
exceptionByName _ "NoSuchPropertyException" msg = Neo4jNoSuchProperty msg
exceptionByName def _ _ = def

-- | Parse batch exceptions
parseException :: L.ByteString -> Neo4jException
parseException b = fromMaybe defaultException $ do
                    parsedobj <- J.decode b
                    msg <- flip JT.parseMaybe parsedobj $ \obj -> (obj .: "message") >>= J.parseJSON 
                    parsedmsg <- J.decode (L.fromStrict $ TE.encodeUtf8 msg)
                    (errName, expl) <- flip JT.parseMaybe parsedmsg $ \obj -> do
                                                                        expl <- obj .: "message"
                                                                        errName <- obj .: "exception"
                                                                        return (errName, expl)
                    return $ exceptionByName defaultException errName expl
    where defaultException = Neo4jBatchException b

runBatch :: Batch a -> Neo4j Graph
runBatch b = Neo4j $ \conn -> do
        res <- httpCreate500Explained conn "/db/data/batch" $ J.encode b
        let arr = case res of
                    Left bodyErr -> throw $ parseException bodyErr
                    Right bodySuc -> bodySuc
        return $ parseBatchResponse arr b
