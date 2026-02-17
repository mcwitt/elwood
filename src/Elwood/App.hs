module Elwood.App
  ( AppEnv (..)
  , runApp
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

import Elwood.Claude.Client
import Elwood.Claude.Conversation
import Elwood.Claude.Handler
import Elwood.Config
import Elwood.Logging
import Elwood.Telegram.Client
import Elwood.Telegram.Polling

-- | Application environment containing all initialized components
data AppEnv = AppEnv
  { appConfig :: Config
  , appLogger :: Logger
  , appTelegram :: TelegramClient
  , appClaude :: ClaudeClient
  , appConversations :: ConversationStore
  , appSystemPrompt :: Maybe Text
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

  let _env =
        AppEnv
          { appConfig = config
          , appLogger = logger
          , appTelegram = telegram
          , appClaude = claude
          , appConversations = conversations
          , appSystemPrompt = systemPrompt
          }

  -- Log allowed chats
  logInfo
    logger
    "Allowed chat IDs"
    [("chats", T.pack (show (cfgAllowedChatIds config)))]

  -- Start polling loop with Claude handler
  runPolling
    logger
    telegram
    (cfgAllowedChatIds config)
    (claudeHandler logger claude conversations systemPrompt (cfgModel config))
