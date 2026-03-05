{-# LANGUAGE TupleSections #-}

module Elwood.MCP.Client
  ( -- * Server Lifecycle
    spawnServer,
    stopServer,

    -- * JSON-RPC Communication
    sendRequest,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forever, void, when)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (Logger, logDebug, logError, logInfo, logWarn)
import Elwood.MCP.Types
import System.Environment (getEnvironment)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBuffering)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
  )
import System.Timeout (timeout)

-- | Response timeout in microseconds (30 seconds)
responseTimeoutMicros :: Int
responseTimeoutMicros = 30 * 1000000

-- | Spawn and initialize an MCP server
spawnServer :: Logger -> MCPServerConfig -> IO (Either MCPError MCPServer)
spawnServer logger cfg = do
  logInfo logger "Spawning MCP server" [("name", cfg.name)]

  -- Merge current environment with MCP server's custom vars
  envPairs <- mergeEnv cfg.env

  -- Create the process
  let processConfig =
        (proc (T.unpack cfg.command) (map T.unpack cfg.args))
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            env = envPairs
          }

  spawnResult <- try $ createProcess processConfig
  case spawnResult of
    Left (e :: SomeException) -> do
      logError logger "Failed to spawn MCP server" [("error", T.pack (show e))]
      pure $ Left $ MCPSpawnError $ T.pack $ show e
    Right (Just stdinH, Just stdoutH, Just stderrH, procHandle) -> do
      -- Set buffering for communication
      hSetBuffering stdinH NoBuffering
      hSetBuffering stdoutH LineBuffering
      hSetBuffering stderrH LineBuffering

      -- Create request ID counter and pending requests map
      requestIdRef <- newIORef 0
      pendingRef <- newIORef Map.empty

      let server =
            MCPServer
              { config = cfg,
                process = procHandle,
                stdin = stdinH,
                stdout = stdoutH,
                stderr = stderrH,
                requestId = requestIdRef,
                pendingRequests = pendingRef
              }

      -- Start background thread to log stderr output
      void $ forkIO $ logStderr logger cfg.name stderrH

      -- Start background thread to read and dispatch responses
      void $ forkIO $ responseReader logger server

      -- Optional startup delay for slow-starting servers (e.g., npx)
      when (cfg.startupDelay > 0) $
        threadDelay (cfg.startupDelay * 1000) -- Convert ms to μs

      -- Check if process exited during startup
      exitCode <- getProcessExitCode procHandle
      case exitCode of
        Just code -> do
          logError
            logger
            "MCP server exited during startup"
            [ ("name", cfg.name),
              ("exit_code", T.pack (show code))
            ]
          stopServer server
          pure $ Left $ MCPSpawnError $ "Server exited with code " <> T.pack (show code)
        Nothing -> do
          initResult <- initializeMCPServer logger server
          case initResult of
            Left err -> do
              logError
                logger
                "MCP server initialization failed"
                [ ("name", cfg.name),
                  ("error", T.pack (show err))
                ]
              stopServer server
              pure $ Left err
            Right () -> do
              logInfo logger "MCP server initialized" [("name", cfg.name)]
              pure $ Right server
    Right _ -> do
      logError logger "Failed to create process pipes" []
      pure $ Left $ MCPSpawnError "Failed to create process pipes"

-- | Merge custom environment variables with current environment
mergeEnv :: Maybe [(Text, Text)] -> IO (Maybe [(String, String)])
mergeEnv Nothing = pure Nothing -- Inherit current environment
mergeEnv (Just customPairs) = do
  currentEnv <- getEnvironment
  let customEnv = [(T.unpack k, T.unpack v) | (k, v) <- customPairs]
      customKeys = map fst customEnv
      filteredCurrent = filter (\(k, _) -> k `notElem` customKeys) currentEnv
  pure $ Just (filteredCurrent ++ customEnv)

-- | Initialize MCP server with handshake
initializeMCPServer :: Logger -> MCPServer -> IO (Either MCPError ())
initializeMCPServer logger server = do
  let initParams =
        object
          [ "protocolVersion" .= ("2024-11-05" :: Text),
            "capabilities" .= object [],
            "clientInfo"
              .= object
                [ "name" .= ("elwood" :: Text),
                  "version" .= ("0.1.0" :: Text)
                ]
          ]

  initResult <- sendRequest server "initialize" (Just initParams)
  case initResult of
    Left err -> pure $ Left err
    Right _ -> do
      sendNotification logger server "notifications/initialized" Nothing
      pure $ Right ()

-- | Send a JSON-RPC notification (no response expected)
sendNotification :: Logger -> MCPServer -> Text -> Maybe Value -> IO ()
sendNotification logger server method_ params_ = do
  let notification =
        object $
          ["jsonrpc" .= ("2.0" :: Text), "method" .= method_]
            ++ maybe [] (\p -> ["params" .= p]) params_

  let jsonLine = BL.toStrict (encode notification) <> "\n"
  logDebug logger "Sending MCP notification" [("method", method_)]

  BS.hPut server.stdin jsonLine
  hFlush server.stdin

-- | Send a JSON-RPC request and wait for response (with timeout)
sendRequest :: MCPServer -> Text -> Maybe Value -> IO (Either MCPError Value)
sendRequest server method_ params_ = do
  reqId <- atomicModifyIORef' server.requestId (\n -> (n + 1, n + 1))

  let request =
        JsonRpcRequest
          { jsonrpc = "2.0",
            method = method_,
            params = params_,
            id_ = reqId
          }

  -- Create MVar and register in pending requests
  mvar <- newEmptyMVar
  atomicModifyIORef' server.pendingRequests (\m -> (Map.insert reqId mvar m, ()))

  let reqBytes = BL.toStrict (encode request) <> "\n"
  sendResult <- try $ do
    BS.hPut server.stdin reqBytes
    hFlush server.stdin

  case sendResult of
    Left (e :: SomeException) -> do
      atomicModifyIORef' server.pendingRequests (\m -> (Map.delete reqId m, ()))
      pure $ Left $ MCPRequestError $ "Failed to send request: " <> T.pack (show e)
    Right () -> do
      -- Wait for the reader thread to dispatch our response
      result <- timeout responseTimeoutMicros (takeMVar mvar)
      case result of
        Nothing -> do
          atomicModifyIORef' server.pendingRequests (\m -> (Map.delete reqId m, ()))
          pure $ Left $ MCPRequestError "Response timeout (30s)"
        Just r -> pure r

-- | Background thread that reads JSON-RPC responses from stdout and dispatches
-- them to pending request MVars by ID. Terminates when stdout is closed.
responseReader :: Logger -> MCPServer -> IO ()
responseReader logger server =
  forever readAndDispatch `catch` \(e :: SomeException) -> do
    logDebug logger "MCP response reader exiting" [("reason", T.pack (show e))]
    -- Signal all pending requests that the server is gone
    pending <- atomicModifyIORef' server.pendingRequests (Map.empty,)
    let err = Left $ MCPRequestError "MCP server connection closed"
    mapM_ (\mv -> void $ tryPutMVar mv err) (Map.elems pending)
  where
    readAndDispatch = do
      lineBytes <- BS.hGetLine server.stdout
      when (isJsonLine lineBytes) $ do
        let line = BL.fromStrict lineBytes
        case eitherDecode line of
          Left _ -> pure ()
          Right response -> dispatchResponse response

    dispatchResponse :: JsonRpcResponse -> IO ()
    dispatchResponse response = case response.id_ of
      Nothing -> pure () -- Notification, ignore
      Just respId -> do
        mMvar <- atomicModifyIORef' server.pendingRequests (\m -> (Map.delete respId m, Map.lookup respId m))
        case mMvar of
          Just mvar -> do
            result <- handleResponse response
            putMVar mvar result
          Nothing ->
            logWarn logger "MCP response for unknown request ID" [("id", T.pack (show respId))]

-- | Check if a line looks like it could be JSON (starts with '{')
isJsonLine :: BS.ByteString -> Bool
isJsonLine bs = case BS.uncons (BS.dropWhile (== ' ') bs) of
  Just ('{', _) -> True
  _ -> False

-- | Handle a parsed JSON-RPC response
handleResponse :: JsonRpcResponse -> IO (Either MCPError Value)
handleResponse response =
  case response.error of
    Just err -> pure $ Left $ MCPToolError err.code err.message
    Nothing -> pure $ Right $ fromMaybe Null response.result

-- | Clean shutdown of an MCP server
stopServer :: MCPServer -> IO ()
stopServer server = do
  hClose server.stdin `catch` ignoreException
  hClose server.stdout `catch` ignoreException
  hClose server.stderr `catch` ignoreException
  terminateProcess server.process
  _ <- waitForProcess server.process
  pure ()
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = pure ()

-- | Background thread that reads stderr from an MCP server and logs each line.
-- Terminates when the handle is closed or the server process exits.
logStderr :: Logger -> Text -> Handle -> IO ()
logStderr logger serverName h =
  forever readAndLog `catch` \(_ :: SomeException) -> pure ()
  where
    readAndLog = do
      line <- BS.hGetLine h
      logWarn logger "MCP server stderr" [("server", serverName), ("message", TE.decodeUtf8Lenient line)]
