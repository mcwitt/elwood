{-# LANGUAGE StrictData #-}

module Elwood.MCP.Client
  ( -- * Server Lifecycle
    spawnMCPServer,
    stopMCPServer,

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
spawnMCPServer :: Logger -> MCPServerConfig -> IO (Either MCPError MCPServer)
spawnMCPServer logger config = do
  logInfo logger "Spawning MCP server" [("name", mscName config)]

  -- Merge current environment with MCP server's custom vars
  envPairs <- mergeEnv (mscEnv config)

  -- Create the process
  let processConfig =
        (proc (T.unpack (mscCommand config)) (map T.unpack (mscArgs config)))
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
    Right (Just stdin, Just stdout, Just stderr, procHandle) -> do
      -- Set buffering for communication
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout LineBuffering
      hSetBuffering stderr LineBuffering

      -- Create request ID counter
      requestIdRef <- newIORef 0

      let server =
            MCPServer
              { msConfig = config,
                msProcess = procHandle,
                msStdin = stdin,
                msStdout = stdout,
                msRequestId = requestIdRef
              }

      -- Optional startup delay for slow-starting servers (e.g., npx)
      when (mscStartupDelay config > 0) $
        threadDelay (mscStartupDelay config * 1000) -- Convert ms to μs

      -- Check if process exited during startup
      exitCode <- getProcessExitCode procHandle
      case exitCode of
        Just code -> do
          stderrContent <- hGetContents stderr `catch` \(_ :: SomeException) -> pure ""
          logError
            logger
            "MCP server exited during startup"
            [ ("name", mscName config),
              ("exit_code", T.pack (show code)),
              ("stderr", T.pack (take 500 stderrContent))
            ]
          pure $ Left $ MCPSpawnError $ "Server exited with code " <> T.pack (show code)
        Nothing -> do
          initResult <- initializeMCPServer logger server
          case initResult of
            Left err -> do
              terminateProcess procHandle
              pure $ Left err
            Right () -> do
              logInfo logger "MCP server initialized" [("name", mscName config)]
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
sendNotification logger server method params = do
  let notification =
        object $
          ["jsonrpc" .= ("2.0" :: Text), "method" .= method]
            ++ maybe [] (\p -> ["params" .= p]) params

  let jsonLine = BL.toStrict (encode notification) <> "\n"
  logDebug logger "Sending MCP notification" [("method", method)]

  hFlush (msStdin server)
  BL.hPut (msStdin server) (BL.fromStrict jsonLine)
  hFlush (msStdin server)

-- | Send a JSON-RPC request and wait for response (with timeout)
sendRequest :: MCPServer -> Text -> Maybe Value -> IO (Either MCPError Value)
sendRequest server method params = do
  reqId <- atomicModifyIORef' (msRequestId server) (\n -> (n + 1, n + 1))

  let request =
        JsonRpcRequest
          { jrqJsonrpc = "2.0",
            jrqMethod = method,
            jrqParams = params,
            jrqId = reqId
          }

  sendResult <- try $ do
    BL8.hPutStrLn (msStdin server) (encode request)
    hFlush (msStdin server)

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
      responseResult <- try $ BS.hGetLine (msStdout server)
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
      case jrsId response of
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
  case jrsError response of
    Just err -> pure $ Left $ MCPToolError (jreCode err) (jreMessage err)
    Nothing -> pure $ Right $ fromMaybe Null (jrsResult response)

-- | Clean shutdown of an MCP server
stopMCPServer :: MCPServer -> IO ()
stopMCPServer server = do
  hClose (msStdin server) `catch` ignoreException
  hClose (msStdout server) `catch` ignoreException
  terminateProcess (msProcess server)
  _ <- waitForProcess (msProcess server)
  pure ()
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = pure ()
