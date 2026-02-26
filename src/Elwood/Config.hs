{-# LANGUAGE StrictData #-}

module Elwood.Config
  ( Config (..),
    CompactionConfig (..),
    MCPServerConfig (..),
    PermissionConfig (..),
    PermissionConfigFile (..),
    loadConfig,
    parseToolSearch,

    -- * Re-exports
    ThinkingLevel (..),
    ThinkingEffort (..),
    parseThinkingLevel,
    WebhookServerConfig (..),
    WebhookConfig (..),
    PromptInput (..),
    PromptInputFile (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (ToolName)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Permissions (PermissionConfig (..), ToolPolicy (..), defaultPermissionConfig)
import Elwood.Prompt (PromptInput (..), PromptInputFile (..), resolvePromptInput)
import Elwood.Thinking (ThinkingEffort (..), ThinkingLevel (..), parseThinkingLevel)
import Elwood.Webhook.Types
  ( DeliveryTargetFile (..),
    WebhookConfig (..),
    WebhookConfigFile (..),
    WebhookServerConfig (..),
    WebhookServerConfigFile (..),
  )
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

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
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled)
    toolSearch :: Maybe [Text],
    -- | System prompt inputs (assembled at startup)
    systemPrompt :: [PromptInput]
  }
  deriving stock (Show, Generic)

-- | Configuration for context compaction
data CompactionConfig = CompactionConfig
  { -- | Compact when estimated tokens exceed this
    tokenThreshold :: Int,
    -- | Model to use for summarization (e.g., "claude-3-5-haiku-20241022")
    model :: Text
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
    thinking :: Maybe ThinkingLevel,
    maxIterations :: Maybe Int,
    toolSearch :: Maybe Value,
    systemPrompt :: Maybe [PromptInputFile]
  }
  deriving stock (Show, Generic)

-- | Permission configuration from YAML file
data PermissionConfigFile = PermissionConfigFile
  { safePatterns :: Maybe [Text],
    dangerousPatterns :: Maybe [Text],
    toolPolicies :: Maybe (Map ToolName ToolPolicy),
    defaultPolicy :: Maybe ToolPolicy,
    approvalTimeoutSeconds :: Maybe Int
  }
  deriving stock (Show, Generic)

-- | Compaction configuration from YAML file
data CompactionConfigFile = CompactionConfigFile
  { tokenThreshold :: Maybe Int,
    model :: Maybe Text
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
    rejectUnknownKeys "ConfigFile" ["state_dir", "workspace_dir", "allowed_chat_ids", "model", "permissions", "compaction", "mcp_servers", "webhook", "thinking", "max_iterations", "tool_search", "dynamic_tool_loading", "system_prompt"] v
    ConfigFile
      <$> v .:? "state_dir"
      <*> v .:? "workspace_dir"
      <*> v .:? "allowed_chat_ids"
      <*> v .:? "model"
      <*> v .:? "permissions"
      <*> v .:? "compaction"
      <*> v .:? "mcp_servers"
      <*> v .:? "webhook"
      <*> v .:? "thinking"
      <*> v .:? "max_iterations"
      <*> (v .:? "tool_search" <|> v .:? "dynamic_tool_loading")
      <*> v .:? "system_prompt"

instance FromJSON PermissionConfigFile where
  parseJSON = withObject "PermissionConfigFile" $ \v -> do
    rejectUnknownKeys "PermissionConfigFile" ["safe_patterns", "dangerous_patterns", "tool_policies", "default_policy", "approval_timeout_seconds"] v
    PermissionConfigFile
      <$> v .:? "safe_patterns"
      <*> v .:? "dangerous_patterns"
      <*> v .:? "tool_policies"
      <*> v .:? "default_policy"
      <*> v .:? "approval_timeout_seconds"

instance FromJSON CompactionConfigFile where
  parseJSON = withObject "CompactionConfigFile" $ \v -> do
    rejectUnknownKeys "CompactionConfigFile" ["token_threshold", "model"] v
    CompactionConfigFile
      <$> v .:? "token_threshold"
      <*> v .:? "model"

instance FromJSON MCPServerConfigFile where
  parseJSON = withObject "MCPServerConfigFile" $ \v -> do
    rejectUnknownKeys "MCPServerConfigFile" ["command", "args", "env", "startup_delay"] v
    MCPServerConfigFile
      <$> v .: "command"
      <*> v .:? "args"
      <*> v .:? "env"
      <*> v .:? "startup_delay"

-- | Default compaction configuration
defaultCompaction :: CompactionConfig
defaultCompaction =
  CompactionConfig
    { tokenThreshold = 50000,
      model = "claude-3-5-haiku-20241022"
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
  let perms = case configFile.permissions of
        Nothing -> defaultPermissionConfig
        Just pcf ->
          PermissionConfig
            { safePatterns = fromMaybe defaultPermissionConfig.safePatterns pcf.safePatterns,
              dangerousPatterns = fromMaybe defaultPermissionConfig.dangerousPatterns pcf.dangerousPatterns,
              toolPolicies = fromMaybe Map.empty pcf.toolPolicies,
              defaultPolicy = fromMaybe defaultPermissionConfig.defaultPolicy pcf.defaultPolicy,
              approvalTimeoutSeconds = fromMaybe defaultPermissionConfig.approvalTimeoutSeconds pcf.approvalTimeoutSeconds
            }

  let compact = case configFile.compaction of
        Nothing -> defaultCompaction
        Just ccf ->
          CompactionConfig
            { tokenThreshold = fromMaybe defaultCompaction.tokenThreshold ccf.tokenThreshold,
              model = fromMaybe defaultCompaction.model ccf.model
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
  let resolveDeliveryTarget :: DeliveryTargetFile -> DeliveryTarget
      resolveDeliveryTarget (DeliveryTargetFileTelegram s) = maybe TelegramBroadcast TelegramDelivery s
      resolveDeliveryTarget DeliveryTargetFileLog = LogOnly

  let workspaceDir_ = fromMaybe "/var/lib/assistant/workspace" configFile.workspaceDir

  -- Webhook secret: env var takes precedence over config file
  let webhookSecret = webhookSecretEnv <|> (configFile.webhook >>= (.secret))

  let webhookCfg = case configFile.webhook of
        Nothing ->
          WebhookServerConfig
            { enabled = False,
              port = 8080,
              secret = webhookSecret,
              webhooks = []
            }
        Just wscf ->
          WebhookServerConfig
            { enabled = fromMaybe False wscf.enabled,
              port = fromMaybe 8080 wscf.port,
              secret = webhookSecret,
              webhooks = case wscf.endpoints of
                Nothing -> []
                Just eps ->
                  [ WebhookConfig
                      { name = ep.name,
                        prompt = maybe [] (map resolvePromptInput) ep.prompt,
                        session = maybe Isolated Named ep.session,
                        deliveryTargets = case ep.deliveryTargets of
                          Nothing -> [TelegramBroadcast]
                          Just targets ->
                            let parsed = map resolveDeliveryTarget targets
                             in if null parsed
                                  then [TelegramBroadcast]
                                  else parsed,
                        suppressIfContains = ep.suppressIfContains,
                        model = ep.model,
                        thinking = parseThinkingLevel <$> ep.thinking
                      }
                  | ep <- eps
                  ]
            }

  let systemPrompt_ = case configFile.systemPrompt of
        Nothing -> [WorkspaceFile "SOUL.md"]
        Just spf -> map resolvePromptInput spf

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
        thinking = fromMaybe ThinkingOff configFile.thinking,
        maxIterations = fromMaybe 20 configFile.maxIterations,
        toolSearch = parseToolSearch =<< configFile.toolSearch,
        systemPrompt = systemPrompt_
      }

-- | Parse tool search configuration from a YAML value
--
-- Supported formats:
--   false / absent         -> Nothing (disabled)
--   true / []              -> Just [] (enabled, all tools deferred)
--   [tool1, tool2]         -> Just [tool1, tool2] (enabled, listed tools never deferred)
--   {alwaysLoad: [...]}    -> Just [...] (backward compat with old dynamicToolLoading format)
parseToolSearch :: Value -> Maybe [Text]
parseToolSearch (Bool False) = Nothing
parseToolSearch (Bool True) = Just []
parseToolSearch (Array arr) = Just [t | String t <- V.toList arr]
parseToolSearch (Object obj) =
  let names = case KM.lookup (Key.fromText "alwaysLoad") obj of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []
   in Just names
parseToolSearch _ = Nothing
