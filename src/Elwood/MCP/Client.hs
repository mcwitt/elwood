{-# LANGUAGE StrictData #-}

module Elwood.MCP.Client
  ( -- * Server Lifecycle
    spawnServer,
    stopServer,

    -- * JSON-RPC Communication
    sendRequest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, try)
import Control.Monad (when)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (Logger, logDebug, logError, logInfo)
import Elwood.MCP.Types
import System.Environment (getEnvironment)
import System.IO (BufferMode (..), hClose, hFlush, hGetContents, hSetBuffering)
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

      -- Create request ID counter
      requestIdRef <- newIORef 0

      let server =
            MCPServer
              { config = cfg,
                process = procHandle,
                stdin = stdinH,
                stdout = stdoutH,
                requestId = requestIdRef
              }

      -- Optional startup delay for slow-starting servers (e.g., npx)
      when (cfg.startupDelay > 0) $
        threadDelay (cfg.startupDelay * 1000) -- Convert ms to μs

      -- Check if process exited during startup
      exitCode <- getProcessExitCode procHandle
      case exitCode of
        Just code -> do
          stderrContent <- hGetContents stderrH `catch` \(_ :: SomeException) -> pure ""
          logError
            logger
            "MCP server exited during startup"
            [ ("name", cfg.name),
              ("exit_code", T.pack (show code)),
              ("stderr", T.pack (take 500 stderrContent))
            ]
          pure $ Left $ MCPSpawnError $ "Server exited with code " <> T.pack (show code)
        Nothing -> do
          initResult <- initializeMCPServer logger server
          case initResult of
            Left err -> do
              -- Capture stderr to help diagnose initialization failures
              stderrContent <- hGetContents stderrH `catch` \(_ :: SomeException) -> pure ""
              logError
                logger
                "MCP server initialization failed"
                [ ("name", cfg.name),
                  ("error", T.pack (show err)),
                  ("stderr", T.pack (take 1000 stderrContent))
                ]
              terminateProcess procHandle
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

  hFlush server.stdin
  BL.hPut server.stdin (BL.fromStrict jsonLine)
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

  sendResult <- try $ do
    BL8.hPutStrLn server.stdin (encode request)
    hFlush server.stdin

  case sendResult of
    Left (e :: SomeException) ->
      pure $ Left $ MCPRequestError $ "Failed to send request: " <> T.pack (show e)
    Right () -> do
      -- Read response with timeout
      result <- timeout responseTimeoutMicros (readResponse server reqId)
      case result of
        Nothing -> pure $ Left $ MCPRequestError "Response timeout (30s)"
        Just r -> pure r

-- | Read and parse JSON-RPC response, skipping non-JSON lines
readResponse :: MCPServer -> Int -> IO (Either MCPError Value)
readResponse server expectedId = readJsonLine 100
  where
    readJsonLine :: Int -> IO (Either MCPError Value)
    readJsonLine 0 = pure $ Left $ MCPProtocolError "No JSON response found after 100 lines"
    readJsonLine remaining = do
      responseResult <- try $ BS.hGetLine server.stdout
      case responseResult of
        Left (e :: SomeException) ->
          pure $ Left $ MCPRequestError $ "Failed to read response: " <> T.pack (show e)
        Right lineBytes -> do
          let line = BL.fromStrict lineBytes
          if not (isJsonLine lineBytes)
            then readJsonLine (remaining - 1)
            else case eitherDecode line of
              Left _ -> readJsonLine (remaining - 1)
              Right response -> validateResponse response

    validateResponse response =
      case response.id_ of
        Just respId
          | respId == expectedId -> handleResponse response
          | otherwise ->
              pure $
                Left $
                  MCPProtocolError $
                    "Response ID mismatch: expected "
                      <> T.pack (show expectedId)
                      <> ", got "
                      <> T.pack (show respId)
        Nothing -> handleResponse response

    -- Check if a line looks like it could be JSON (starts with '{')
    isJsonLine bs = case BS.uncons (BS.dropWhile (== 0x20) bs) of -- 0x20 = space
      Just (0x7B, _) -> True -- 0x7B = '{'
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
  terminateProcess server.process
  _ <- waitForProcess server.process
  pure ()
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = pure ()
