module Elwood.MCP.Client (spawnServer, stopServer, sendRequest, defaultRequestTimeoutSeconds) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally, mask, onException)
import Control.Monad (forever, void, when)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Elwood.Config (MCPServerConfig (..))
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logWarn)
import Elwood.MCP.Types
import System.Environment (getEnvironment)
import System.IO
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process
import System.Timeout (timeout)

defaultRequestTimeoutSeconds :: Int
defaultRequestTimeoutSeconds = 30

sendRequest :: MCPServer -> Int -> Text -> Maybe Value -> IO (Either MCPError Value)
sendRequest = (.request)

stopServer :: MCPServer -> IO ()
stopServer = (.close)

spawnServer :: Logger -> MCPServerConfig -> IO (Either MCPError MCPServer)
spawnServer logger config = (Right <$> acquire) `catchSync` (pure . Left . MCPSpawnError . T.pack . show)
  where
    acquire = mask $ \restore -> do
      environment <- getEnvironment
      let custom = [(T.unpack key, T.unpack value) | (key, value) <- fromMaybe [] config.env]
          environment' = Map.toList (Map.union (Map.fromList custom) (Map.fromList environment))
      process <- createProcess (proc (T.unpack config.command) (map T.unpack config.args)) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just environment', create_group = True}
      case process of
        (Just input, Just output, Just errors, handle) -> do
          group <- getPid handle
          stopped <- newMVar False
          let stop = modifyMVar_ stopped $ \done -> if done then pure True else stopProcess handle group >> pure True
              cleanup = stop >> mapM_ (\h -> hClose h `catchSync` const (pure ())) [input, output, errors]
          ( do
              hSetBuffering input NoBuffering
              pending <- newTVarIO (Just Map.empty)
              counter <- newTVarIO 0
              writer <- newMVar ()
              let closed = MCPRequestError "MCP server connection closed"
                  closePending = atomically $ do
                    entries <- readTVar pending
                    writeTVar pending Nothing
                    mapM_ (\box -> void (tryPutTMVar box (Left closed))) (maybe [] Map.elems entries)
                  write value = withMVar writer $ \() -> do
                    open <- readTVarIO pending
                    case open of
                      Nothing -> ioError (userError "MCP connection closed")
                      Just _ -> (BL.hPut input (encode value <> "\n") >> hFlush input) `onException` (closePending >> stop)
                  request seconds method params = mask $ \unmask -> do
                    box <- newEmptyTMVarIO
                    registration <- atomically $ do
                      entries <- readTVar pending
                      case entries of
                        Nothing -> pure Nothing
                        Just entries' -> do
                          key <- (+ 1) <$> readTVar counter
                          writeTVar counter key
                          writeTVar pending (Just (Map.insert key box entries'))
                          pure (Just key)
                    case registration of
                      Nothing -> pure (Left closed)
                      Just key ->
                        let cleanupRequest = atomically $ modifyTVar' pending (fmap (Map.delete key))
                            exchange = do
                              write (toJSON (JsonRpcRequest "2.0" method params key))
                              atomically (readTMVar box)
                            microseconds = fromInteger (max 1 (min (toInteger (maxBound :: Int)) (toInteger seconds * 1000000)))
                         in ( unmask (fromMaybe (Left (MCPRequestError "Response timeout")) <$> timeout microseconds exchange)
                                `catchSync` (pure . Left . MCPRequestError . T.pack . show)
                            )
                              `finally` cleanupRequest
                  readResponse = do
                    bytes <- BS.hGetLine output
                    case eitherDecodeStrict @JsonRpcResponse bytes of
                      Left _ -> pure ()
                      Right response -> case response.id_ of
                        Nothing -> pure ()
                        Just key -> atomically $ do
                          entries <- readTVar pending
                          case entries >>= Map.lookup key of
                            Nothing -> pure ()
                            Just box -> do
                              let result = case (response.error, response.result) of
                                    (Just err, _) -> Left (MCPToolError err.code err.message)
                                    (Nothing, Just value) -> Right value
                                    _ -> Left (MCPProtocolError "Response has neither result nor error")
                              void (tryPutTMVar box result)
                              modifyTVar' pending (fmap (Map.delete key))
                  report exception = logWarn logger "MCP worker stopped" [("reason", T.pack (show exception))]
              reader <- Async.async (restore (forever readResponse `catchSync` report) `finally` closePending)
              stderrWorker <- Async.async (restore (forever (BS.hGetLine errors >>= \line -> logWarn logger "MCP stderr" [("message", T.decodeUtf8Lenient line)]) `catchSync` report)) `onException` Async.cancel reader
              let close = closePending >> stop >> mapM_ Async.cancel [reader, stderrWorker] >> mapM_ (\h -> hClose h `catchSync` const (pure ())) [input, output, errors]
                  server = MCPServer request close
                  initialize = do
                    when (config.startupDelay > 0) (threadDelay (fromInteger (min (toInteger (maxBound :: Int)) (toInteger config.startupDelay * 1000))))
                    response <- request 30 "initialize" (Just (object ["protocolVersion" .= ("2024-11-05" :: Text), "capabilities" .= object [], "clientInfo" .= object ["name" .= ("elwood" :: Text), "version" .= ("0.1.0" :: Text)]]))
                    either (ioError . userError . show) (const (write (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]))) response
              restore initialize `onException` close
              pure server
            )
            `onException` cleanup
        _ -> cleanupProcess process >> ioError (userError "MCP process pipes unavailable")

stopProcess :: ProcessHandle -> Maybe ProcessID -> IO ()
stopProcess handle group = do
  mapM_ (\pid -> signalProcessGroup sigTERM pid `catchSync` const (pure ())) group
  terminateProcess handle `catchSync` const (pure ())
  void (timeout 1000000 (waitForProcess handle))
  -- Reap the entire owned group even when its leader exited first.
  mapM_ (\pid -> signalProcessGroup sigKILL pid `catchSync` const (pure ())) group
  void (timeout 1000000 (waitForProcess handle))
