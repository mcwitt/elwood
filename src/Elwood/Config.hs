{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..),
    HeartbeatConfig (..),
    CompactionConfig (..),
    CronJob (..),
    MCPServerConfig (..),
    PermissionConfigFile (..),
    loadConfig,
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Elwood.Permissions (PermissionConfig (..), defaultPermissionConfig)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- | Main configuration for Elwood
data Config = Config
  { -- | Directory for persistent state
    cfgStateDir :: FilePath,
    -- | Directory containing SOUL.md, AGENTS.md, etc.
    cfgWorkspaceDir :: FilePath,
    -- | Telegram bot token (loaded from environment)
    cfgTelegramToken :: Text,
    -- | Anthropic API key (loaded from environment)
    cfgAnthropicApiKey :: Text,
    -- | Brave Search API key (optional, from environment)
    cfgBraveApiKey :: Maybe Text,
    -- | List of chat IDs allowed to interact with the bot
    cfgAllowedChatIds :: [Int64],
    -- | Claude model to use (e.g., "claude-sonnet-4-20250514")
    cfgModel :: Text,
    -- | Maximum messages to keep per conversation
    cfgMaxHistory :: Int,
    -- | Heartbeat/proactive check configuration
    cfgHeartbeat :: HeartbeatConfig,
    -- | Permission configuration for tools
    cfgPermissions :: PermissionConfig,
    -- | Context compaction configuration
    cfgCompaction :: CompactionConfig,
    -- | Scheduled cron jobs
    cfgCronJobs :: [CronJob],
    -- | MCP server configurations
    cfgMCPServers :: [MCPServerConfig]
  }
  deriving stock (Show, Generic)

-- | Configuration for heartbeat/proactive functionality
data HeartbeatConfig = HeartbeatConfig
  { -- | Whether heartbeat is enabled
    hbEnabled :: Bool,
    -- | How often to check for proactive actions (in minutes)
    hbIntervalMinutes :: Int,
    -- | Start of active hours (0-23)
    hbActiveHoursStart :: Int,
    -- | End of active hours (0-23)
    hbActiveHoursEnd :: Int
  }
  deriving stock (Show, Generic)

-- | Configuration for a scheduled cron job
data CronJob = CronJob
  { -- | Job identifier
    cjName :: Text,
    -- | Simple interval (no cron expressions for M5)
    cjIntervalMinutes :: Int,
    -- | Message to send to agent
    cjPrompt :: Text,
    -- | Use fresh session (True) or shared heartbeat session (False)
    cjIsolated :: Bool
  }
  deriving stock (Show, Generic)

-- | Configuration for context compaction
data CompactionConfig = CompactionConfig
  { -- | Compact when estimated tokens exceed this
    ccTokenThreshold :: Int,
    -- | Model to use for summarization (e.g., "claude-3-5-haiku-20241022")
    ccCompactionModel :: Text
  }
  deriving stock (Show, Generic)

-- | Configuration for an MCP server
data MCPServerConfig = MCPServerConfig
  { -- | Server identifier for namespacing tools
    mscName :: Text,
    -- | Command to run (e.g., "npx")
    mscCommand :: Text,
    -- | Command arguments
    mscArgs :: [Text],
    -- | Optional environment variables
    mscEnv :: Maybe [(Text, Text)],
    -- | Milliseconds to wait after spawning before sending initialize (default: 0)
    mscStartupDelay :: Int
  }
  deriving stock (Show, Generic)

-- | YAML file configuration (without secrets)
data ConfigFile = ConfigFile
  { cfStateDir :: Maybe FilePath,
    cfWorkspaceDir :: Maybe FilePath,
    cfAllowedChatIds :: Maybe [Int64],
    cfModel :: Maybe Text,
    cfMaxHistory :: Maybe Int,
    cfHeartbeat :: Maybe HeartbeatConfigFile,
    cfPermissions :: Maybe PermissionConfigFile,
    cfCompaction :: Maybe CompactionConfigFile,
    cfCronJobs :: Maybe [CronJobFile],
    cfMCPServers :: Maybe (Map Text MCPServerConfigFile)
  }
  deriving stock (Show, Generic)

data HeartbeatConfigFile = HeartbeatConfigFile
  { hcEnabled :: Maybe Bool,
    hcIntervalMinutes :: Maybe Int,
    hcActiveHoursStart :: Maybe Int,
    hcActiveHoursEnd :: Maybe Int
  }
  deriving stock (Show, Generic)

data CronJobFile = CronJobFile
  { cjfName :: Text,
    cjfIntervalMinutes :: Int,
    cjfPrompt :: Text,
    cjfIsolated :: Maybe Bool
  }
  deriving stock (Show, Generic)

-- | Permission configuration from YAML file
data PermissionConfigFile = PermissionConfigFile
  { pcfSafeCommands :: Maybe [Text],
    pcfDangerousPatterns :: Maybe [Text],
    pcfAllowedPaths :: Maybe [FilePath]
  }
  deriving stock (Show, Generic)

-- | Compaction configuration from YAML file
data CompactionConfigFile = CompactionConfigFile
  { ccfTokenThreshold :: Maybe Int,
    ccfCompactionModel :: Maybe Text
  }
  deriving stock (Show, Generic)

-- | MCP server configuration from YAML file
data MCPServerConfigFile = MCPServerConfigFile
  { mcfCommand :: Text,
    mcfArgs :: Maybe [Text],
    mcfEnv :: Maybe (Map Text Text),
    mcfStartupDelay :: Maybe Int
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
      <*> v .:? "cronJobs"
      <*> v .:? "mcpServers"

instance FromJSON HeartbeatConfigFile where
  parseJSON = withObject "HeartbeatConfigFile" $ \v ->
    HeartbeatConfigFile
      <$> v .:? "enabled"
      <*> v .:? "intervalMinutes"
      <*> v .:? "activeHoursStart"
      <*> v .:? "activeHoursEnd"

instance FromJSON CronJobFile where
  parseJSON = withObject "CronJobFile" $ \v ->
    CronJobFile
      <$> v .: "name"
      <*> v .: "intervalMinutes"
      <*> v .: "prompt"
      <*> v .:? "isolated"

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

instance FromJSON MCPServerConfigFile where
  parseJSON = withObject "MCPServerConfigFile" $ \v ->
    MCPServerConfigFile
      <$> v .: "command"
      <*> v .:? "args"
      <*> v .:? "env"
      <*> v .:? "startupDelay"

-- | Default heartbeat configuration
defaultHeartbeat :: HeartbeatConfig
defaultHeartbeat =
  HeartbeatConfig
    { hbEnabled = True,
      hbIntervalMinutes = 30,
      hbActiveHoursStart = 8,
      hbActiveHoursEnd = 22
    }

-- | Default compaction configuration
defaultCompaction :: CompactionConfig
defaultCompaction =
  CompactionConfig
    { ccTokenThreshold = 80000,
      ccCompactionModel = "claude-3-5-haiku-20241022"
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
            { hbEnabled = fromMaybe (hbEnabled defaultHeartbeat) (hcEnabled hbf),
              hbIntervalMinutes = fromMaybe (hbIntervalMinutes defaultHeartbeat) (hcIntervalMinutes hbf),
              hbActiveHoursStart = fromMaybe (hbActiveHoursStart defaultHeartbeat) (hcActiveHoursStart hbf),
              hbActiveHoursEnd = fromMaybe (hbActiveHoursEnd defaultHeartbeat) (hcActiveHoursEnd hbf)
            }

  let cronJobs = case cfCronJobs configFile of
        Nothing -> []
        Just cjfs ->
          [ CronJob
              { cjName = cjfName cjf,
                cjIntervalMinutes = cjfIntervalMinutes cjf,
                cjPrompt = cjfPrompt cjf,
                cjIsolated = fromMaybe False (cjfIsolated cjf)
              }
          | cjf <- cjfs
          ]

  let permissions = case cfPermissions configFile of
        Nothing -> defaultPermissionConfig
        Just pcf ->
          PermissionConfig
            { pcSafeCommands = fromMaybe (pcSafeCommands defaultPermissionConfig) (pcfSafeCommands pcf),
              pcDangerousPatterns = fromMaybe (pcDangerousPatterns defaultPermissionConfig) (pcfDangerousPatterns pcf),
              pcAllowedPaths = fromMaybe (pcAllowedPaths defaultPermissionConfig) (pcfAllowedPaths pcf)
            }

  let compaction = case cfCompaction configFile of
        Nothing -> defaultCompaction
        Just ccf ->
          CompactionConfig
            { ccTokenThreshold = fromMaybe (ccTokenThreshold defaultCompaction) (ccfTokenThreshold ccf),
              ccCompactionModel = fromMaybe (ccCompactionModel defaultCompaction) (ccfCompactionModel ccf)
            }

  let mcpServers = case cfMCPServers configFile of
        Nothing -> []
        Just serverMap ->
          [ MCPServerConfig
              { mscName = name,
                mscCommand = mcfCommand mcf,
                mscArgs = fromMaybe [] (mcfArgs mcf),
                mscEnv = Map.toList <$> mcfEnv mcf,
                mscStartupDelay = fromMaybe 0 (mcfStartupDelay mcf)
              }
          | (name, mcf) <- Map.toList serverMap
          ]

  pure
    Config
      { cfgStateDir = fromMaybe "/var/lib/assistant" (cfStateDir configFile),
        cfgWorkspaceDir = fromMaybe "/var/lib/assistant/workspace" (cfWorkspaceDir configFile),
        cfgTelegramToken = telegramToken,
        cfgAnthropicApiKey = anthropicApiKey,
        cfgBraveApiKey = braveApiKey,
        cfgAllowedChatIds = fromMaybe [] (cfAllowedChatIds configFile),
        cfgModel = fromMaybe "claude-sonnet-4-20250514" (cfModel configFile),
        cfgMaxHistory = fromMaybe 50 (cfMaxHistory configFile),
        cfgHeartbeat = heartbeat,
        cfgPermissions = permissions,
        cfgCompaction = compaction,
        cfgCronJobs = cronJobs,
        cfgMCPServers = mcpServers
      }
