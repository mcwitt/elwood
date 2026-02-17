{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..)
  , HeartbeatConfig (..)
  , CompactionConfig (..)
  , PermissionConfigFile (..)
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

import Elwood.Permissions (PermissionConfig (..), defaultPermissionConfig)

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
  , cfgBraveApiKey :: Maybe Text
  -- ^ Brave Search API key (optional, from environment)
  , cfgAllowedChatIds :: [Int64]
  -- ^ List of chat IDs allowed to interact with the bot
  , cfgModel :: Text
  -- ^ Claude model to use (e.g., "claude-sonnet-4-20250514")
  , cfgMaxHistory :: Int
  -- ^ Maximum messages to keep per conversation
  , cfgHeartbeat :: HeartbeatConfig
  -- ^ Heartbeat/proactive check configuration
  , cfgPermissions :: PermissionConfig
  -- ^ Permission configuration for tools
  , cfgCompaction :: CompactionConfig
  -- ^ Context compaction configuration
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

-- | Configuration for context compaction
data CompactionConfig = CompactionConfig
  { ccTokenThreshold :: Int
  -- ^ Compact when estimated tokens exceed this
  , ccCompactionModel :: Text
  -- ^ Model to use for summarization (e.g., "claude-3-5-haiku-20241022")
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
  , cfPermissions :: Maybe PermissionConfigFile
  , cfCompaction :: Maybe CompactionConfigFile
  }
  deriving stock (Show, Generic)

data HeartbeatConfigFile = HeartbeatConfigFile
  { hcIntervalMinutes :: Maybe Int
  , hcActiveHoursStart :: Maybe Int
  , hcActiveHoursEnd :: Maybe Int
  }
  deriving stock (Show, Generic)

-- | Permission configuration from YAML file
data PermissionConfigFile = PermissionConfigFile
  { pcfSafeCommands :: Maybe [Text]
  , pcfDangerousPatterns :: Maybe [Text]
  , pcfAllowedPaths :: Maybe [FilePath]
  }
  deriving stock (Show, Generic)

-- | Compaction configuration from YAML file
data CompactionConfigFile = CompactionConfigFile
  { ccfTokenThreshold :: Maybe Int
  , ccfCompactionModel :: Maybe Text
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
      <*> v .:? "permissions"
      <*> v .:? "compaction"

instance FromJSON HeartbeatConfigFile where
  parseJSON = withObject "HeartbeatConfigFile" $ \v ->
    HeartbeatConfigFile
      <$> v .:? "intervalMinutes"
      <*> v .:? "activeHoursStart"
      <*> v .:? "activeHoursEnd"

instance FromJSON PermissionConfigFile where
  parseJSON = withObject "PermissionConfigFile" $ \v ->
    PermissionConfigFile
      <$> v .:? "safeCommands"
      <*> v .:? "dangerousPatterns"
      <*> v .:? "allowedPaths"

instance FromJSON CompactionConfigFile where
  parseJSON = withObject "CompactionConfigFile" $ \v ->
    CompactionConfigFile
      <$> v .:? "tokenThreshold"
      <*> v .:? "compactionModel"

-- | Default heartbeat configuration
defaultHeartbeat :: HeartbeatConfig
defaultHeartbeat =
  HeartbeatConfig
    { hbIntervalMinutes = 30
    , hbActiveHoursStart = 8
    , hbActiveHoursEnd = 22
    }

-- | Default compaction configuration
defaultCompaction :: CompactionConfig
defaultCompaction =
  CompactionConfig
    { ccTokenThreshold = 80000
    , ccCompactionModel = "claude-3-5-haiku-20241022"
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

  -- Load Brave Search API key from environment (optional)
  braveApiKey <- fmap T.pack <$> lookupEnv "BRAVE_SEARCH_API_KEY"

  -- Build final config with defaults
  let heartbeat = case cfHeartbeat configFile of
        Nothing -> defaultHeartbeat
        Just hbf ->
          HeartbeatConfig
            { hbIntervalMinutes = fromMaybe (hbIntervalMinutes defaultHeartbeat) (hcIntervalMinutes hbf)
            , hbActiveHoursStart = fromMaybe (hbActiveHoursStart defaultHeartbeat) (hcActiveHoursStart hbf)
            , hbActiveHoursEnd = fromMaybe (hbActiveHoursEnd defaultHeartbeat) (hcActiveHoursEnd hbf)
            }

  let permissions = case cfPermissions configFile of
        Nothing -> defaultPermissionConfig
        Just pcf ->
          PermissionConfig
            { pcSafeCommands = fromMaybe (pcSafeCommands defaultPermissionConfig) (pcfSafeCommands pcf)
            , pcDangerousPatterns = fromMaybe (pcDangerousPatterns defaultPermissionConfig) (pcfDangerousPatterns pcf)
            , pcAllowedPaths = fromMaybe (pcAllowedPaths defaultPermissionConfig) (pcfAllowedPaths pcf)
            }

  let compaction = case cfCompaction configFile of
        Nothing -> defaultCompaction
        Just ccf ->
          CompactionConfig
            { ccTokenThreshold = fromMaybe (ccTokenThreshold defaultCompaction) (ccfTokenThreshold ccf)
            , ccCompactionModel = fromMaybe (ccCompactionModel defaultCompaction) (ccfCompactionModel ccf)
            }

  pure
    Config
      { cfgStateDir = fromMaybe "/var/lib/assistant" (cfStateDir configFile)
      , cfgWorkspaceDir = fromMaybe "/var/lib/assistant/workspace" (cfWorkspaceDir configFile)
      , cfgTelegramToken = telegramToken
      , cfgAnthropicApiKey = anthropicApiKey
      , cfgBraveApiKey = braveApiKey
      , cfgAllowedChatIds = fromMaybe [] (cfAllowedChatIds configFile)
      , cfgModel = fromMaybe "claude-sonnet-4-20250514" (cfModel configFile)
      , cfgMaxHistory = fromMaybe 50 (cfMaxHistory configFile)
      , cfgHeartbeat = heartbeat
      , cfgPermissions = permissions
      , cfgCompaction = compaction
      }
