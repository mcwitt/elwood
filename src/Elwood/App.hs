module Elwood.App
  ( AppEnv (..)
  , runApp
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing)

import Elwood.Claude.Client
import Elwood.Claude.Conversation
import Elwood.Claude.Handler
import Elwood.Config
import Elwood.Logging
import Elwood.Permissions (newPermissionChecker)
import Elwood.Telegram.Client
import Elwood.Telegram.Polling
import Elwood.Tools.Command (runCommandTool)
import Elwood.Tools.FileSystem (readFileTool, writeFileTool)
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

  -- Create tool environment
  let toolEnv =
        ToolEnv
          { teLogger = logger
          , teWorkspaceDir = cfgWorkspaceDir config
          , teStateDir = cfgStateDir config
          , tePermissions = permChecker
          , teHttpManager = httpManager
          , teBraveApiKey = cfgBraveApiKey config
          }

  -- Initialize tool registry with built-in tools
  let registry =
        registerTool runCommandTool $
        registerTool readFileTool $
        registerTool writeFileTool $
        registerTool webSearchTool $
        registerTool webFetchTool $
        newToolRegistry

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

  -- Start polling loop with Claude handler
  runPolling
    logger
    telegram
    (cfgAllowedChatIds config)
    (claudeHandler logger claude conversations registry toolEnv systemPrompt (cfgModel config))
