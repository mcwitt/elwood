module Elwood.App
  ( AppEnv (..)
  , runApp
  ) where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (SomeException, catch, finally)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Elwood.Claude.Client
import Elwood.Claude.Conversation
import Elwood.Claude.Handler
import Elwood.Config
import Elwood.Logging
import Elwood.MCP.Client (stopMCPServer)
import Elwood.MCP.Registry (startMCPServers)
import Elwood.Memory (newMemoryStore)
import Elwood.Permissions (newPermissionChecker)
import Elwood.Scheduler (SchedulerEnv (..), runScheduler)
import Elwood.Telegram.Client
import Elwood.Telegram.Polling
import Elwood.Tools.Command (runCommandTool)
import Elwood.Tools.FileSystem (readFileTool, writeFileTool)
import Elwood.Tools.Memory (saveMemoryTool, searchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types (ToolEnv (..))
import Elwood.Tools.Web (webSearchTool, webFetchTool)

-- | Application environment containing all initialized components
data AppEnv = AppEnv
  { appConfig :: Config
  , appLogger :: Logger
  , appTelegram :: TelegramClient
  , appClaude :: ClaudeClient
  , appConversations :: ConversationStore
  , appSystemPrompt :: Maybe Text
  , appToolRegistry :: ToolRegistry
  , appToolEnv :: ToolEnv
  }

-- | Initialize and run the application
runApp :: Config -> IO ()
runApp config = do
  -- Initialize logging
  logger <- newLogger Info

  logInfo logger "Elwood starting up" []
  logInfo logger "Configuration loaded" [("state_dir", T.pack (cfgStateDir config))]

  -- Ensure state directory exists
  createDirectoryIfMissing True (cfgStateDir config)

  -- Initialize Telegram client
  telegram <- newTelegramClient (cfgTelegramToken config)
  logInfo logger "Telegram client initialized" []

  -- Initialize Claude client
  claude <- newClaudeClient (cfgAnthropicApiKey config)
  logInfo logger "Claude client initialized" [("model", cfgModel config)]

  -- Initialize conversation store
  conversations <- newConversationStore (cfgStateDir config) (cfgMaxHistory config)
  logInfo
    logger
    "Conversation store initialized"
    [("max_history", T.pack (show (cfgMaxHistory config)))]

  -- Load system prompt
  systemPrompt <- loadSystemPrompt (cfgWorkspaceDir config)
  case systemPrompt of
    Just _ -> logInfo logger "System prompt loaded from SOUL.md" []
    Nothing -> logWarn logger "No SOUL.md found, running without system prompt" []

  -- Initialize permission checker
  let permChecker = newPermissionChecker (cfgPermissions config) (cfgWorkspaceDir config)
  logInfo logger "Permission checker initialized" []

  -- Initialize HTTP manager for web tools
  httpManager <- newManager tlsManagerSettings

  -- Initialize memory store
  memoryStore <- newMemoryStore (cfgStateDir config)
  logInfo logger "Memory store initialized" []

  -- Create tool environment
  let toolEnv =
        ToolEnv
          { teLogger = logger
          , teWorkspaceDir = cfgWorkspaceDir config
          , teStateDir = cfgStateDir config
          , tePermissions = permChecker
          , teHttpManager = httpManager
          , teBraveApiKey = cfgBraveApiKey config
          , teMemoryStore = memoryStore
          }

  -- Initialize tool registry with built-in tools
  let builtinRegistry =
        registerTool runCommandTool $
        registerTool readFileTool $
        registerTool writeFileTool $
        registerTool webSearchTool $
        registerTool webFetchTool $
        registerTool saveMemoryTool $
        registerTool searchMemoryTool $
        newToolRegistry

  logInfo
    logger
    "Built-in tools registered"
    [("tool_count", T.pack (show (length (allTools builtinRegistry))))]

  -- Initialize MCP servers and merge tools
  (registry, mcpServers) <- startMCPServers logger (cfgMCPServers config) builtinRegistry

  logInfo
    logger
    "MCP initialized"
    [("servers", T.pack (show (length mcpServers)))]

  -- Get compaction config from main config
  let compactionConfig = cfgCompaction config

  logInfo
    logger
    "Tool registry initialized"
    [("tool_count", T.pack (show (length (allTools registry))))]

  let _env =
        AppEnv
          { appConfig = config
          , appLogger = logger
          , appTelegram = telegram
          , appClaude = claude
          , appConversations = conversations
          , appSystemPrompt = systemPrompt
          , appToolRegistry = registry
          , appToolEnv = toolEnv
          }

  -- Log allowed chats
  logInfo
    logger
    "Allowed chat IDs"
    [("chats", T.pack (show (cfgAllowedChatIds config)))]

  -- Log Brave API key status
  case cfgBraveApiKey config of
    Just _ -> logInfo logger "Brave Search API key configured" []
    Nothing -> logWarn logger "No Brave Search API key, web_search will be unavailable" []

  -- Load HEARTBEAT.md
  heartbeatPrompt <- loadHeartbeatPrompt (cfgWorkspaceDir config)
  case heartbeatPrompt of
    Just _ -> logInfo logger "Heartbeat prompt loaded from HEARTBEAT.md" []
    Nothing -> logInfo logger "No HEARTBEAT.md found, heartbeat will be inactive" []

  -- Log cron jobs
  let cronJobs = cfgCronJobs config
  logInfo logger "Cron jobs configured" [("count", T.pack (show (length cronJobs)))]

  -- Create scheduler environment
  let schedulerEnv =
        SchedulerEnv
          { seLogger = logger
          , seTelegram = telegram
          , seClaude = claude
          , seConversations = conversations
          , seRegistry = registry
          , seToolEnv = toolEnv
          , seCompaction = compactionConfig
          , seSystemPrompt = systemPrompt
          , seModel = cfgModel config
          , seHeartbeatConfig = cfgHeartbeat config
          , seHeartbeatPrompt = heartbeatPrompt
          , seNotifyChatIds = cfgAllowedChatIds config
          , seCronJobs = cronJobs
          }

  -- Run polling and scheduler concurrently, with MCP cleanup on exit
  finally
    (concurrently_
      (runPolling
        logger
        telegram
        (cfgAllowedChatIds config)
        (claudeHandler logger claude conversations registry toolEnv compactionConfig systemPrompt (cfgModel config)))
      (runScheduler schedulerEnv))
    (do
      logInfo logger "Shutting down MCP servers" []
      mapM_ stopMCPServer mcpServers)

-- | Load heartbeat prompt from HEARTBEAT.md file
loadHeartbeatPrompt :: FilePath -> IO (Maybe Text)
loadHeartbeatPrompt workspaceDir = do
  let heartbeatPath = workspaceDir </> "HEARTBEAT.md"
  exists <- doesFileExist heartbeatPath
  if exists
    then do
      content <- TIO.readFile heartbeatPath
        `catch` \(_ :: SomeException) -> pure ""
      if T.null content
        then pure Nothing
        else pure (Just content)
    else pure Nothing
