{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..),
    CompactionConfig (..),
    MCPServerConfig (..),
    ThinkingLevel (..),
    ThinkingEffort (..),
    DynamicToolLoadingConfig (..),
    PermissionConfig (..),
    PermissionConfigFile (..),
    loadConfig,
    parseDynamicToolLoading,
    parseThinkingLevel,

    -- * Re-exports for webhook config
    WebhookServerConfig (..),
    WebhookConfig (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
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
import System.FilePath ((</>))

-- | Extended thinking level for Claude
data ThinkingLevel
  = ThinkingOff
  | ThinkingAdaptive ThinkingEffort
  | -- | Explicit budget_tokens (for older models)
    ThinkingBudget Int
  deriving stock (Show, Eq, Generic)

-- | Effort level for adaptive thinking
data ThinkingEffort = EffortLow | EffortMedium | EffortHigh
  deriving stock (Show, Eq, Generic)

-- | Configuration for dynamic tool loading
newtype DynamicToolLoadingConfig = DynamicToolLoadingConfig
  { alwaysLoad :: [Text]
  }
  deriving stock (Show, Eq)

-- | Parse a thinking level from a YAML value
--
-- Supported formats:
--   off / false          -> ThinkingOff (YAML parses bare "off" as boolean False)
--   {type: off}          -> ThinkingOff
--   {type: adaptive, effort: low}    -> ThinkingAdaptive EffortLow
--   {type: adaptive, effort: medium} -> ThinkingAdaptive EffortMedium
--   {type: adaptive, effort: high}   -> ThinkingAdaptive EffortHigh
--   {type: fixed, budgetTokens: N}   -> ThinkingBudget N
parseThinkingLevel :: Value -> ThinkingLevel
parseThinkingLevel (Bool False) = ThinkingOff
parseThinkingLevel (String t)
  | T.toLower t == "off" = ThinkingOff
parseThinkingLevel (Object obj) = case lookupText "type" obj of
  Just "adaptive" -> case lookupText "effort" obj of
    Just "low" -> ThinkingAdaptive EffortLow
    Just "medium" -> ThinkingAdaptive EffortMedium
    Just "high" -> ThinkingAdaptive EffortHigh
    _ -> ThinkingAdaptive EffortMedium -- default effort
  Just "fixed" -> case KM.lookup (Key.fromText "budgetTokens") obj of
    Just (Number n) | n > 0 -> ThinkingBudget (round n)
    _ -> ThinkingOff
  Just "off" -> ThinkingOff
  _ -> ThinkingOff
parseThinkingLevel _ = ThinkingOff

-- | Look up a text value in a KeyMap
lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
  Just (String t) -> Just (T.toLower t)
  _ -> Nothing

-- | Main configuration for Elwood
data Config = Config
  { -- | Directory for persistent state
    stateDir :: FilePath,
    -- | Directory containing SOUL.md, AGENTS.md, etc.
    workspaceDir :: FilePath,
    -- | Telegram bot token (loaded from environment)
    telegramToken :: Text,
    -- | Anthropic API key (loaded from environment)
    anthropicApiKey :: Text,
    -- | List of chat IDs allowed to interact with the bot
    allowedChatIds :: [Int64],
    -- | Claude model to use (e.g., "claude-sonnet-4-20250514")
    model :: Text,
    -- | Permission configuration for tools
    permissions :: PermissionConfig,
    -- | Context compaction configuration
    compaction :: CompactionConfig,
    -- | MCP server configurations
    mcpServers :: [MCPServerConfig],
    -- | Webhook server configuration
    webhook :: WebhookServerConfig,
    -- | Extended thinking level
    thinking :: ThinkingLevel,
    -- | Maximum agent loop iterations per turn (prevents infinite tool-use loops)
    maxIterations :: Int,
    -- | Dynamic tool loading (Nothing = disabled, Just cfg = enabled)
    dynamicToolLoading :: Maybe DynamicToolLoadingConfig
  }
  deriving stock (Show, Generic)

-- | Configuration for context compaction
data CompactionConfig = CompactionConfig
  { -- | Compact when estimated tokens exceed this
    tokenThreshold :: Int,
    -- | Model to use for summarization (e.g., "claude-3-5-haiku-20241022")
    compactionModel :: Text
  }
  deriving stock (Show, Generic)

-- | Configuration for an MCP server
data MCPServerConfig = MCPServerConfig
  { -- | Server identifier for namespacing tools
    name :: Text,
    -- | Command to run (e.g., "npx")
    command :: Text,
    -- | Command arguments
    args :: [Text],
    -- | Optional environment variables
    env :: Maybe [(Text, Text)],
    -- | Milliseconds to wait after spawning before sending initialize (default: 0)
    startupDelay :: Int
  }
  deriving stock (Show, Generic)

-- | YAML file configuration (without secrets)
data ConfigFile = ConfigFile
  { stateDir :: Maybe FilePath,
    workspaceDir :: Maybe FilePath,
    allowedChatIds :: Maybe [Int64],
    model :: Maybe Text,
    permissions :: Maybe PermissionConfigFile,
    compaction :: Maybe CompactionConfigFile,
    mcpServers :: Maybe (Map Text MCPServerConfigFile),
    webhook :: Maybe WebhookServerConfigFile,
    thinking :: Maybe Value,
    maxIterations :: Maybe Int,
    dynamicToolLoading :: Maybe Value
  }
  deriving stock (Show, Generic)

-- | Permission configuration from YAML file
data PermissionConfigFile = PermissionConfigFile
  { safeCommands :: Maybe [Text],
    dangerousPatterns :: Maybe [Text],
    toolPolicies :: Maybe (Map Text Text),
    defaultPolicy :: Maybe Text,
    approvalTimeoutSeconds :: Maybe Int
  }
  deriving stock (Show, Generic)

-- | Compaction configuration from YAML file
data CompactionConfigFile = CompactionConfigFile
  { tokenThreshold :: Maybe Int,
    compactionModel :: Maybe Text
  }
  deriving stock (Show, Generic)

-- | MCP server configuration from YAML file
data MCPServerConfigFile = MCPServerConfigFile
  { command :: Text,
    args :: Maybe [Text],
    env :: Maybe (Map Text Text),
    startupDelay :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v -> do
    rejectUnknownKeys "ConfigFile" ["stateDir", "workspaceDir", "allowedChatIds", "model", "permissions", "compaction", "mcpServers", "webhook", "thinking", "maxIterations", "dynamicToolLoading"] v
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
      <*> v .:? "maxIterations"
      <*> v .:? "dynamicToolLoading"

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
    { tokenThreshold = 50000,
      compactionModel = "claude-3-5-haiku-20241022"
    }

-- | Load configuration from a YAML file and environment variables
loadConfig :: FilePath -> IO Config
loadConfig path = do
  -- Load YAML file
  configFile <- Yaml.decodeFileThrow path :: IO ConfigFile

  -- Load Telegram token from environment (required)
  telegramToken_ <-
    lookupEnv "TELEGRAM_BOT_TOKEN" >>= \case
      Nothing -> fail "TELEGRAM_BOT_TOKEN environment variable is required"
      Just t -> pure (T.pack t)

  -- Load Anthropic API key from environment (required)
  anthropicApiKey_ <-
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

  let perms = case configFile.permissions of
        Nothing -> defaultPermissionConfig
        Just pcf ->
          PermissionConfig
            { safeCommands = fromMaybe defaultPermissionConfig.safeCommands pcf.safeCommands,
              dangerousPatterns = fromMaybe defaultPermissionConfig.dangerousPatterns pcf.dangerousPatterns,
              toolPolicies = maybe Map.empty (Map.mapMaybe parseToolPolicy) pcf.toolPolicies,
              defaultPolicy = maybe defaultPermissionConfig.defaultPolicy parseToolPolicyOrDefault pcf.defaultPolicy,
              approvalTimeoutSeconds = fromMaybe defaultPermissionConfig.approvalTimeoutSeconds pcf.approvalTimeoutSeconds
            }

  let compact = case configFile.compaction of
        Nothing -> defaultCompaction
        Just ccf ->
          CompactionConfig
            { tokenThreshold = fromMaybe defaultCompaction.tokenThreshold ccf.tokenThreshold,
              compactionModel = fromMaybe defaultCompaction.compactionModel ccf.compactionModel
            }

  let servers = case configFile.mcpServers of
        Nothing -> []
        Just serverMap ->
          [ MCPServerConfig
              { name = n,
                command = mcf.command,
                args = fromMaybe [] mcf.args,
                env = Map.toList <$> mcf.env,
                startupDelay = fromMaybe 0 mcf.startupDelay
              }
          | (n, mcf) <- Map.toList serverMap
          ]

  -- Helper to resolve delivery targets from file objects
  let resolveDeliveryTarget :: DeliveryTargetFile -> Maybe DeliveryTarget
      resolveDeliveryTarget dtf = case T.toLower dtf.type_ of
        "telegram" -> Just $ maybe TelegramBroadcast TelegramDelivery dtf.session
        "log" -> Just LogOnly
        _ -> Nothing

  let workspaceDir_ = fromMaybe "/var/lib/assistant/workspace" configFile.workspaceDir

  -- Webhook secret: env var takes precedence over config file
  let webhookGlobalSecret = webhookSecretEnv <|> (configFile.webhook >>= (.globalSecret))

  let webhookCfg = case configFile.webhook of
        Nothing ->
          WebhookServerConfig
            { enabled = False,
              port = 8080,
              globalSecret = webhookGlobalSecret,
              webhooks = []
            }
        Just wscf ->
          WebhookServerConfig
            { enabled = fromMaybe False wscf.enabled,
              port = fromMaybe 8080 wscf.port,
              globalSecret = webhookGlobalSecret,
              webhooks = case wscf.endpoints of
                Nothing -> []
                Just eps ->
                  [ WebhookConfig
                      { name = ep.name,
                        secret = ep.secret,
                        promptTemplate = ep.promptTemplate,
                        promptFile = fmap (workspaceDir_ </>) ep.promptFile,
                        session = maybe Isolated Named ep.session,
                        delivery = case ep.deliver of
                          Nothing -> [TelegramBroadcast]
                          Just targets ->
                            let parsed = mapMaybe resolveDeliveryTarget targets
                             in if null parsed
                                  then [TelegramBroadcast]
                                  else parsed,
                        suppressIfContains = ep.suppressIfContains,
                        model = ep.model,
                        thinking = ep.thinking
                      }
                  | ep <- eps
                  ]
            }

  pure
    Config
      { stateDir = fromMaybe "/var/lib/assistant" configFile.stateDir,
        workspaceDir = workspaceDir_,
        telegramToken = telegramToken_,
        anthropicApiKey = anthropicApiKey_,
        allowedChatIds = fromMaybe [] configFile.allowedChatIds,
        model = fromMaybe "claude-sonnet-4-20250514" configFile.model,
        permissions = perms,
        compaction = compact,
        mcpServers = servers,
        webhook = webhookCfg,
        thinking = maybe ThinkingOff parseThinkingLevel configFile.thinking,
        maxIterations = fromMaybe 30 configFile.maxIterations,
        dynamicToolLoading = parseDynamicToolLoading =<< configFile.dynamicToolLoading
      }

-- | Parse dynamic tool loading from a YAML value
--
-- Supported formats:
--   false / absent     -> Nothing (disabled)
--   true               -> Just (DynamicToolLoadingConfig [])
--   {alwaysLoad: [...]} -> Just (DynamicToolLoadingConfig [...])
parseDynamicToolLoading :: Value -> Maybe DynamicToolLoadingConfig
parseDynamicToolLoading (Bool False) = Nothing
parseDynamicToolLoading (Bool True) = Just (DynamicToolLoadingConfig [])
parseDynamicToolLoading (Object obj) =
  let names = case KM.lookup (Key.fromText "alwaysLoad") obj of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []
   in Just (DynamicToolLoadingConfig names)
parseDynamicToolLoading _ = Nothing
