{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..),
    CompactionConfig (..),
    MCPServerConfig (..),
    ThinkingLevel (..),
    PermissionConfigFile (..),
    loadConfig,

    -- * Re-exports for webhook config
    WebhookServerConfig (..),
    WebhookConfig (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Permissions (PermissionConfig (..), ToolPolicy (..), defaultPermissionConfig)
import Elwood.Webhook.Types
  ( DeliveryTargetFile (..),
    WebhookConfig (..),
    WebhookConfigFile (..),
    WebhookServerConfig (..),
    WebhookServerConfigFile (..),
  )
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- | Extended thinking level for Claude
data ThinkingLevel
  = ThinkingOff
  | ThinkingLow
  | ThinkingMedium
  | ThinkingHigh
  deriving stock (Show, Eq, Generic)

-- | Parse a thinking level from a YAML value
-- YAML parses "off" as boolean False, so we handle both Text and Bool
parseThinkingLevel :: Value -> ThinkingLevel
parseThinkingLevel (String t) = case T.toLower t of
  "low" -> ThinkingLow
  "medium" -> ThinkingMedium
  "high" -> ThinkingHigh
  _ -> ThinkingOff
parseThinkingLevel (Bool False) = ThinkingOff
parseThinkingLevel _ = ThinkingOff

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
    -- | List of chat IDs allowed to interact with the bot
    cfgAllowedChatIds :: [Int64],
    -- | Claude model to use (e.g., "claude-sonnet-4-20250514")
    cfgModel :: Text,
    -- | Permission configuration for tools
    cfgPermissions :: PermissionConfig,
    -- | Context compaction configuration
    cfgCompaction :: CompactionConfig,
    -- | MCP server configurations
    cfgMCPServers :: [MCPServerConfig],
    -- | Webhook server configuration
    cfgWebhook :: WebhookServerConfig,
    -- | Extended thinking level
    cfgThinking :: ThinkingLevel
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
    cfPermissions :: Maybe PermissionConfigFile,
    cfCompaction :: Maybe CompactionConfigFile,
    cfMCPServers :: Maybe (Map Text MCPServerConfigFile),
    cfWebhook :: Maybe WebhookServerConfigFile,
    cfThinking :: Maybe Value
  }
  deriving stock (Show, Generic)

-- | Permission configuration from YAML file
data PermissionConfigFile = PermissionConfigFile
  { pcfSafeCommands :: Maybe [Text],
    pcfDangerousPatterns :: Maybe [Text],
    pcfToolPolicies :: Maybe (Map Text Text),
    pcfDefaultPolicy :: Maybe Text,
    pcfApprovalTimeoutSeconds :: Maybe Int
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
  parseJSON = withObject "ConfigFile" $ \v -> do
    rejectUnknownKeys "ConfigFile" ["stateDir", "workspaceDir", "allowedChatIds", "model", "permissions", "compaction", "mcpServers", "webhook", "thinking"] v
    ConfigFile
      <$> v .:? "stateDir"
      <*> v .:? "workspaceDir"
      <*> v .:? "allowedChatIds"
      <*> v .:? "model"
      <*> v .:? "permissions"
      <*> v .:? "compaction"
      <*> v .:? "mcpServers"
      <*> v .:? "webhook"
      <*> v .:? "thinking"

instance FromJSON PermissionConfigFile where
  parseJSON = withObject "PermissionConfigFile" $ \v -> do
    rejectUnknownKeys "PermissionConfigFile" ["safeCommands", "dangerousPatterns", "toolPolicies", "defaultPolicy", "approvalTimeoutSeconds"] v
    PermissionConfigFile
      <$> v .:? "safeCommands"
      <*> v .:? "dangerousPatterns"
      <*> v .:? "toolPolicies"
      <*> v .:? "defaultPolicy"
      <*> v .:? "approvalTimeoutSeconds"

instance FromJSON CompactionConfigFile where
  parseJSON = withObject "CompactionConfigFile" $ \v -> do
    rejectUnknownKeys "CompactionConfigFile" ["tokenThreshold", "compactionModel"] v
    CompactionConfigFile
      <$> v .:? "tokenThreshold"
      <*> v .:? "compactionModel"

instance FromJSON MCPServerConfigFile where
  parseJSON = withObject "MCPServerConfigFile" $ \v -> do
    rejectUnknownKeys "MCPServerConfigFile" ["command", "args", "env", "startupDelay"] v
    MCPServerConfigFile
      <$> v .: "command"
      <*> v .:? "args"
      <*> v .:? "env"
      <*> v .:? "startupDelay"

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

  -- Load webhook secret from environment (optional, overrides config file)
  webhookSecretEnv <- fmap T.pack <$> lookupEnv "WEBHOOK_SECRET"

  -- Build final config with defaults
  -- Helper to parse tool policy from string
  let parseToolPolicy :: Text -> Maybe ToolPolicy
      parseToolPolicy t = case T.toLower t of
        "allow" -> Just PolicyAllow
        "ask" -> Just PolicyAsk
        "deny" -> Just PolicyDeny
        _ -> Nothing

      parseToolPolicyOrDefault :: Text -> ToolPolicy
      parseToolPolicyOrDefault t = fromMaybe PolicyAllow (parseToolPolicy t)

  let permissions = case cfPermissions configFile of
        Nothing -> defaultPermissionConfig
        Just pcf ->
          PermissionConfig
            { pcSafeCommands = fromMaybe (pcSafeCommands defaultPermissionConfig) (pcfSafeCommands pcf),
              pcDangerousPatterns = fromMaybe (pcDangerousPatterns defaultPermissionConfig) (pcfDangerousPatterns pcf),
              pcToolPolicies = maybe Map.empty (Map.mapMaybe parseToolPolicy) (pcfToolPolicies pcf),
              pcDefaultPolicy = maybe (pcDefaultPolicy defaultPermissionConfig) parseToolPolicyOrDefault (pcfDefaultPolicy pcf),
              pcApprovalTimeoutSeconds = fromMaybe (pcApprovalTimeoutSeconds defaultPermissionConfig) (pcfApprovalTimeoutSeconds pcf)
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

  -- Helper to resolve delivery targets from file objects
  let resolveDeliveryTarget :: DeliveryTargetFile -> Maybe DeliveryTarget
      resolveDeliveryTarget dtf = case T.toLower (dtfType dtf) of
        "telegram" -> Just $ maybe TelegramBroadcast TelegramDelivery (dtfSession dtf)
        "log" -> Just LogOnly
        _ -> Nothing

  -- Webhook secret: env var takes precedence over config file
  let webhookGlobalSecret = webhookSecretEnv <|> (cfWebhook configFile >>= wscfGlobalSecret)

  let webhook = case cfWebhook configFile of
        Nothing -> defaultWebhookServerConfig {wscGlobalSecret = webhookGlobalSecret}
        Just wscf ->
          WebhookServerConfig
            { wscEnabled = fromMaybe False (wscfEnabled wscf),
              wscPort = fromMaybe 8080 (wscfPort wscf),
              wscGlobalSecret = webhookGlobalSecret,
              wscWebhooks = case wscfEndpoints wscf of
                Nothing -> []
                Just endpoints ->
                  [ WebhookConfig
                      { wcName = wcfName ep,
                        wcSecret = wcfSecret ep,
                        wcPromptTemplate = wcfPromptTemplate ep,
                        wcPromptFile = wcfPromptFile ep,
                        wcSession = maybe Isolated Named (wcfSession ep),
                        wcDelivery = case wcfDeliver ep of
                          Nothing -> [TelegramBroadcast]
                          Just targets ->
                            let parsed = mapMaybe resolveDeliveryTarget targets
                             in if null parsed
                                  then [TelegramBroadcast]
                                  else parsed,
                        wcSuppressIfContains = wcfSuppressIfContains ep
                      }
                  | ep <- endpoints
                  ]
            }

  pure
    Config
      { cfgStateDir = fromMaybe "/var/lib/assistant" (cfStateDir configFile),
        cfgWorkspaceDir = fromMaybe "/var/lib/assistant/workspace" (cfWorkspaceDir configFile),
        cfgTelegramToken = telegramToken,
        cfgAnthropicApiKey = anthropicApiKey,
        cfgAllowedChatIds = fromMaybe [] (cfAllowedChatIds configFile),
        cfgModel = fromMaybe "claude-sonnet-4-20250514" (cfModel configFile),
        cfgPermissions = permissions,
        cfgCompaction = compaction,
        cfgMCPServers = mcpServers,
        cfgWebhook = webhook,
        cfgThinking = maybe ThinkingOff parseThinkingLevel (cfThinking configFile)
      }

-- | Default webhook server configuration (disabled)
defaultWebhookServerConfig :: WebhookServerConfig
defaultWebhookServerConfig =
  WebhookServerConfig
    { wscEnabled = False,
      wscPort = 8080,
      wscGlobalSecret = Nothing,
      wscWebhooks = []
    }
