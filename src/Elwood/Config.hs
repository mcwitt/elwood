{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..)
  , HeartbeatConfig (..)
  , loadConfig
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- | Main configuration for Elwood
data Config = Config
  { cfgStateDir :: FilePath
  -- ^ Directory for persistent state
  , cfgWorkspaceDir :: FilePath
  -- ^ Directory containing SOUL.md, AGENTS.md, etc.
  , cfgTelegramToken :: Text
  -- ^ Telegram bot token (loaded from environment)
  , cfgAnthropicApiKey :: Text
  -- ^ Anthropic API key (loaded from environment)
  , cfgAllowedChatIds :: [Int64]
  -- ^ List of chat IDs allowed to interact with the bot
  , cfgModel :: Text
  -- ^ Claude model to use (e.g., "claude-sonnet-4-20250514")
  , cfgMaxHistory :: Int
  -- ^ Maximum messages to keep per conversation
  , cfgHeartbeat :: HeartbeatConfig
  -- ^ Heartbeat/proactive check configuration
  }
  deriving stock (Show, Generic)

-- | Configuration for heartbeat/proactive functionality
data HeartbeatConfig = HeartbeatConfig
  { hbIntervalMinutes :: Int
  -- ^ How often to check for proactive actions (in minutes)
  , hbActiveHoursStart :: Int
  -- ^ Start of active hours (0-23)
  , hbActiveHoursEnd :: Int
  -- ^ End of active hours (0-23)
  }
  deriving stock (Show, Generic)

-- | YAML file configuration (without secrets)
data ConfigFile = ConfigFile
  { cfStateDir :: Maybe FilePath
  , cfWorkspaceDir :: Maybe FilePath
  , cfAllowedChatIds :: Maybe [Int64]
  , cfModel :: Maybe Text
  , cfMaxHistory :: Maybe Int
  , cfHeartbeat :: Maybe HeartbeatConfigFile
  }
  deriving stock (Show, Generic)

data HeartbeatConfigFile = HeartbeatConfigFile
  { hcIntervalMinutes :: Maybe Int
  , hcActiveHoursStart :: Maybe Int
  , hcActiveHoursEnd :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v ->
    ConfigFile
      <$> v .:? "stateDir"
      <*> v .:? "workspaceDir"
      <*> v .:? "allowedChatIds"
      <*> v .:? "model"
      <*> v .:? "maxHistory"
      <*> v .:? "heartbeat"

instance FromJSON HeartbeatConfigFile where
  parseJSON = withObject "HeartbeatConfigFile" $ \v ->
    HeartbeatConfigFile
      <$> v .:? "intervalMinutes"
      <*> v .:? "activeHoursStart"
      <*> v .:? "activeHoursEnd"

-- | Default heartbeat configuration
defaultHeartbeat :: HeartbeatConfig
defaultHeartbeat =
  HeartbeatConfig
    { hbIntervalMinutes = 30
    , hbActiveHoursStart = 8
    , hbActiveHoursEnd = 22
    }

-- | Load configuration from a YAML file and environment variables
loadConfig :: FilePath -> IO Config
loadConfig path = do
  -- Load YAML file
  configFile <- Yaml.decodeFileThrow path :: IO ConfigFile

  -- Load Telegram token from environment (required)
  telegramToken <-
    lookupEnv "TELEGRAM_BOT_TOKEN" >>= \case
      Nothing -> fail "TELEGRAM_BOT_TOKEN environment variable is required"
      Just t -> pure (T.pack t)

  -- Load Anthropic API key from environment (required)
  anthropicApiKey <-
    lookupEnv "ANTHROPIC_API_KEY" >>= \case
      Nothing -> fail "ANTHROPIC_API_KEY environment variable is required"
      Just k -> pure (T.pack k)

  -- Build final config with defaults
  let heartbeat = case cfHeartbeat configFile of
        Nothing -> defaultHeartbeat
        Just hbf ->
          HeartbeatConfig
            { hbIntervalMinutes = fromMaybe (hbIntervalMinutes defaultHeartbeat) (hcIntervalMinutes hbf)
            , hbActiveHoursStart = fromMaybe (hbActiveHoursStart defaultHeartbeat) (hcActiveHoursStart hbf)
            , hbActiveHoursEnd = fromMaybe (hbActiveHoursEnd defaultHeartbeat) (hcActiveHoursEnd hbf)
            }

  pure
    Config
      { cfgStateDir = fromMaybe "/var/lib/assistant" (cfStateDir configFile)
      , cfgWorkspaceDir = fromMaybe "/var/lib/assistant/workspace" (cfWorkspaceDir configFile)
      , cfgTelegramToken = telegramToken
      , cfgAnthropicApiKey = anthropicApiKey
      , cfgAllowedChatIds = fromMaybe [] (cfAllowedChatIds configFile)
      , cfgModel = fromMaybe "claude-sonnet-4-20250514" (cfModel configFile)
      , cfgMaxHistory = fromMaybe 50 (cfMaxHistory configFile)
      , cfgHeartbeat = heartbeat
      }
