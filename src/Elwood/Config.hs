module Elwood.Config
  ( Config (..),
    TelegramChatConfig (..),
    CompactionConfig (..),
    CompactionStrategy (..),
    PruningStrategy (..),
    PruningConfig (..),
    ThinkingPruningConfig (..),
    ToolPruningConfig (..),
    ToolDirectionConfig (..),
    PruningConfigFile (..),
    ThinkingPruningConfigFile (..),
    ToolPruningConfigFile (..),
    ToolDirectionConfigFile (..),
    resolvePruning,
    MCPServerConfig (..),
    PermissionConfig (..),
    PermissionConfigFile (..),
    loadConfig,
  )
where

-- Note: PermissionConfig and PermissionConfigFile are re-exported from Elwood.Permissions

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.AgentSettings
  ( AgentOverrides (..),
    AgentPreset (..),
    AgentProfile (..),
    resolveProfile,
  )
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Permissions (PermissionConfig (..), PermissionConfigFile (..))
import Elwood.Positive (Positive, unsafePositive)
import Elwood.Webhook.Types
  ( DeliveryTargetFile (..),
    WebhookConfig (..),
    WebhookConfigFile (..),
    WebhookServerConfig (..),
    WebhookServerConfigFile (..),
  )
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)

-- | Strategy for pruning: how many recent turns to protect from modification.
-- 'KeepTurns 0' is valid (protect nothing).
data PruningStrategy
  = -- | Keep last N turns
    KeepTurns Natural
  | -- | Keep a fraction of total turns
    KeepFraction Double
  deriving stock (Show, Eq, Generic)

instance FromJSON PruningStrategy where
  parseJSON = withObject "PruningStrategy" $ \v ->
    case KM.keys v of
      ["keep_turns"] -> KeepTurns <$> v .: "keep_turns"
      ["keep_fraction"] -> do
        f <- v .: "keep_fraction"
        when (f <= 0 || f > 1) $ fail "keep_fraction must be in (0, 1]"
        pure (KeepFraction f)
      _ -> fail "PruningStrategy must have exactly one key: keep_turns or keep_fraction"

-- | Strategy for compaction: how many recent turns to keep verbatim.
-- 'CKeepTurns' uses 'Positive' because compaction must always keep at least
-- one turn (keeping zero turns would discard the entire conversation).
data CompactionStrategy
  = -- | Keep last N turns (N > 0)
    CKeepTurns Positive
  | -- | Keep a fraction of total turns (converted via ceiling, like pruning)
    CKeepFraction Double
  deriving stock (Show, Eq, Generic)

instance FromJSON CompactionStrategy where
  parseJSON = withObject "CompactionStrategy" $ \v ->
    case KM.keys v of
      ["keep_turns"] -> CKeepTurns <$> v .: "keep_turns"
      ["keep_fraction"] -> do
        f <- v .: "keep_fraction"
        when (f <= 0 || f > 1) $ fail "keep_fraction must be in (0, 1]"
        pure (CKeepFraction f)
      _ -> fail "CompactionStrategy must have exactly one key: keep_turns or keep_fraction"

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
    -- | Telegram chat configurations (ID + session)
    telegramChats :: [TelegramChatConfig],
    -- | Resolved agent profile (model, thinking, permissions, etc.)
    agentProfile :: AgentProfile,
    -- | Context compaction configuration
    compaction :: CompactionConfig,
    -- | Tool result pruning configuration
    pruning :: PruningConfig,
    -- | MCP server configurations
    mcpServers :: [MCPServerConfig],
    -- | Webhook server configuration
    webhook :: WebhookServerConfig,
    -- | Send notification messages when the agent uses tools
    toolUseMessages :: Bool,
    -- | Delegate sub-agent preset (overrides + optional description)
    delegateAgent :: AgentPreset,
    -- | Named agent presets for delegate_task
    delegateExtraAgents :: Map Text AgentPreset,
    -- | Allowed models for delegate_task tool parameter
    delegateAllowedModels :: [Text],
    -- | Maximum image dimension (Nothing = disabled, Just n = resize to n)
    maxImageDimension :: Maybe Int
  }
  deriving stock (Show, Generic)

-- | Configuration for context compaction
data CompactionConfig = CompactionConfig
  { -- | Compact when estimated tokens exceed this
    tokenThreshold :: Positive,
    -- | Model to use for summarization (e.g., "claude-3-5-haiku-20241022")
    model :: Text,
    -- | Custom compaction prompt (Nothing = use built-in structured default)
    prompt :: Maybe Text,
    -- | Strategy for splitting messages into compact vs keep regions
    strategy :: CompactionStrategy
  }
  deriving stock (Show, Generic)

-- | Configuration for tool result pruning
data PruningConfig = PruningConfig
  { -- | Global default retention strategy for pruning
    strategy :: PruningStrategy,
    -- | Thinking block pruning (Nothing = keep all thinking blocks)
    thinking :: Maybe ThinkingPruningConfig,
    -- | Tool result/input pruning configuration
    tools :: ToolPruningConfig
  }
  deriving stock (Show, Generic)

-- | Configuration for thinking block pruning
newtype ThinkingPruningConfig = ThinkingPruningConfig
  { -- | Retention strategy for thinking block pruning
    strategy :: PruningStrategy
  }
  deriving stock (Show, Eq, Generic)

-- | Configuration for tool content pruning (head/tail chars, per-direction overrides)
data ToolPruningConfig = ToolPruningConfig
  { -- | Characters to keep from the start of pruned content
    headChars :: Int,
    -- | Characters to keep from the end of pruned content
    tailChars :: Int,
    -- | Retention strategy for tools (cascaded to directions as default)
    strategy :: PruningStrategy,
    -- | Resolved configuration for tool inputs
    input :: ToolDirectionConfig,
    -- | Resolved configuration for tool outputs
    output :: ToolDirectionConfig
  }
  deriving stock (Show, Generic)

-- | Per-direction (input/output) pruning configuration
data ToolDirectionConfig = ToolDirectionConfig
  { -- | Retention strategy for this direction
    strategy :: PruningStrategy,
    -- | Characters to keep from the start
    headChars :: Int,
    -- | Characters to keep from the end
    tailChars :: Int
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

-- | Telegram chat configuration from YAML file
data TelegramChatConfigFile = TelegramChatConfigFile
  { id_ :: Int64,
    session :: Maybe Text,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Generic)

-- | Resolved telegram chat configuration
data TelegramChatConfig = TelegramChatConfig
  { id_ :: Int64,
    session :: SessionConfig,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Eq, Generic)

-- | Channels configuration from YAML file
newtype ChannelsConfigFile = ChannelsConfigFile
  { telegram :: Maybe [TelegramChatConfigFile]
  }
  deriving stock (Show, Generic)

-- | YAML file configuration (without secrets)
data ConfigFile = ConfigFile
  { stateDir :: Maybe FilePath,
    workspaceDir :: Maybe FilePath,
    channels :: Maybe ChannelsConfigFile,
    agentOverrides :: AgentOverrides,
    compaction :: Maybe CompactionConfigFile,
    pruning :: Maybe PruningConfigFile,
    mcpServers :: Maybe (Map Text MCPServerConfigFile),
    webhook :: Maybe WebhookServerConfigFile,
    toolUseMessages :: Maybe Bool,
    delegate :: Maybe DelegateConfigFile,
    -- | Outer Nothing = absent (use default); Just Nothing = explicit null (disabled);
    -- Just (Just n) = explicit value
    maxImageDimension :: Maybe (Maybe Int)
  }
  deriving stock (Show, Generic)

-- | Delegate sub-agent configuration from YAML file
data DelegateConfigFile = DelegateConfigFile
  { agent :: AgentPreset,
    extraAgents :: Map Text AgentPreset,
    allowedModels :: Maybe [Text]
  }
  deriving stock (Show, Generic)

-- | Compaction configuration from YAML file
data CompactionConfigFile = CompactionConfigFile
  { tokenThreshold :: Maybe Positive,
    model :: Maybe Text,
    prompt :: Maybe Text,
    strategy :: Maybe CompactionStrategy
  }
  deriving stock (Show, Generic)

-- | Resolve a partial compaction config using 'fromMaybe' for each field.
resolveCompaction :: CompactionConfigFile -> CompactionConfig
resolveCompaction ccf =
  CompactionConfig
    { tokenThreshold = fromMaybe (unsafePositive 50000) ccf.tokenThreshold,
      model = fromMaybe "claude-3-5-haiku-20241022" ccf.model,
      prompt = ccf.prompt,
      strategy = fromMaybe (CKeepTurns (unsafePositive 10)) ccf.strategy
    }

-- | Pruning configuration from YAML file
data PruningConfigFile = PruningConfigFile
  { strategy :: Maybe PruningStrategy,
    -- | Outer 'Maybe' = "not specified in this layer";
    -- inner 'Maybe' = "explicitly set to null (disable thinking pruning)".
    thinking :: Maybe (Maybe ThinkingPruningConfigFile),
    tools :: Maybe ToolPruningConfigFile
  }
  deriving stock (Show, Generic)

-- | Thinking pruning configuration from YAML file
newtype ThinkingPruningConfigFile = ThinkingPruningConfigFile
  { strategy :: Maybe PruningStrategy
  }
  deriving stock (Show, Eq, Generic)

-- | Tool pruning configuration from YAML file
data ToolPruningConfigFile = ToolPruningConfigFile
  { headChars :: Maybe Int,
    tailChars :: Maybe Int,
    strategy :: Maybe PruningStrategy,
    input :: Maybe ToolDirectionConfigFile,
    output :: Maybe ToolDirectionConfigFile
  }
  deriving stock (Show, Eq, Generic)

-- | Per-direction tool pruning configuration from YAML file
data ToolDirectionConfigFile = ToolDirectionConfigFile
  { strategy :: Maybe PruningStrategy,
    headChars :: Maybe Int,
    tailChars :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

-- | Right-biased: @a <> b@ picks @b@'s values when 'Just'.
instance Semigroup PruningConfigFile where
  a <> b =
    PruningConfigFile
      { strategy = b.strategy <|> a.strategy,
        thinking = b.thinking <|> a.thinking,
        tools = b.tools <|> a.tools
      }

instance Monoid PruningConfigFile where
  mempty = PruningConfigFile Nothing Nothing Nothing

-- | Default pruning overrides (all 'Just' with hardcoded defaults).
pruningDefaults :: PruningConfigFile
pruningDefaults =
  PruningConfigFile
    { strategy = Just (KeepTurns 3),
      thinking = Just (Just (ThinkingPruningConfigFile Nothing)),
      tools = Nothing
    }

-- | Resolve a partial pruning config by layering over 'pruningDefaults'.
resolvePruning :: PruningConfigFile -> PruningConfig
resolvePruning pcf =
  let d = pruningDefaults <> pcf
      globalStrategy = fromMaybe (KeepTurns 3) d.strategy
      thinkingCfg = case fromMaybe (Just (ThinkingPruningConfigFile Nothing)) d.thinking of
        Nothing -> Nothing
        Just tcf -> Just ThinkingPruningConfig {strategy = fromMaybe globalStrategy tcf.strategy}
      toolsCf = fromMaybe (ToolPruningConfigFile Nothing Nothing Nothing Nothing Nothing) d.tools
      toolsHeadChars = fromMaybe 500 toolsCf.headChars
      toolsTailChars = fromMaybe 500 toolsCf.tailChars
      toolsStrategy = fromMaybe globalStrategy toolsCf.strategy
      mkDirection dcf =
        let dc = fromMaybe (ToolDirectionConfigFile Nothing Nothing Nothing) dcf
         in ToolDirectionConfig
              { strategy = fromMaybe toolsStrategy dc.strategy,
                headChars = fromMaybe toolsHeadChars dc.headChars,
                tailChars = fromMaybe toolsTailChars dc.tailChars
              }
   in PruningConfig
        { strategy = globalStrategy,
          thinking = thinkingCfg,
          tools =
            ToolPruningConfig
              { headChars = toolsHeadChars,
                tailChars = toolsTailChars,
                strategy = toolsStrategy,
                input = mkDirection toolsCf.input,
                output = mkDirection toolsCf.output
              }
        }

-- | MCP server configuration from YAML file
data MCPServerConfigFile = MCPServerConfigFile
  { command :: Text,
    args :: Maybe [Text],
    env :: Maybe (Map Text Text),
    startupDelay :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromJSON TelegramChatConfigFile where
  parseJSON = withObject "TelegramChatConfigFile" $ \v -> do
    rejectUnknownKeys "TelegramChatConfigFile" ["id", "session", "agent"] v
    TelegramChatConfigFile
      <$> v .: "id"
      <*> v .:? "session"
      <*> v .:? "agent" .!= mempty

instance FromJSON ChannelsConfigFile where
  parseJSON = withObject "ChannelsConfigFile" $ \v -> do
    rejectUnknownKeys "ChannelsConfigFile" ["telegram"] v
    ChannelsConfigFile <$> v .:? "telegram"

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v -> do
    rejectUnknownKeys "ConfigFile" ["state_dir", "workspace", "channels", "agent", "compaction", "pruning", "mcp_servers", "webhook", "tool_use_messages", "delegate", "max_image_dimension"] v
    ConfigFile
      <$> v .:? "state_dir"
      <*> v .:? "workspace"
      <*> v .:? "channels"
      <*> v .:? "agent" .!= mempty
      <*> v .:? "compaction"
      <*> v .:? "pruning"
      <*> v .:? "mcp_servers"
      <*> v .:? "webhook"
      <*> v .:? "tool_use_messages"
      <*> v .:? "delegate"
      <*> v .:? "max_image_dimension"

instance FromJSON CompactionConfigFile where
  parseJSON = withObject "CompactionConfigFile" $ \v -> do
    rejectUnknownKeys "CompactionConfigFile" ["token_threshold", "model", "prompt", "strategy"] v
    CompactionConfigFile
      <$> v .:? "token_threshold"
      <*> v .:? "model"
      <*> v .:? "prompt"
      <*> v .:? "strategy"

instance FromJSON PruningConfigFile where
  parseJSON = withObject "PruningConfigFile" $ \v -> do
    rejectUnknownKeys "PruningConfigFile" ["strategy", "thinking", "tools"] v
    PruningConfigFile
      <$> v .:? "strategy"
      <*> v .:! "thinking"
      <*> v .:? "tools"

instance FromJSON ThinkingPruningConfigFile where
  parseJSON = withObject "ThinkingPruningConfigFile" $ \v -> do
    rejectUnknownKeys "ThinkingPruningConfigFile" ["strategy"] v
    ThinkingPruningConfigFile <$> v .:? "strategy"

instance FromJSON ToolPruningConfigFile where
  parseJSON = withObject "ToolPruningConfigFile" $ \v -> do
    rejectUnknownKeys "ToolPruningConfigFile" ["head_chars", "tail_chars", "strategy", "input", "output"] v
    ToolPruningConfigFile
      <$> v .:? "head_chars"
      <*> v .:? "tail_chars"
      <*> v .:? "strategy"
      <*> v .:? "input"
      <*> v .:? "output"

instance FromJSON ToolDirectionConfigFile where
  parseJSON = withObject "ToolDirectionConfigFile" $ \v -> do
    rejectUnknownKeys "ToolDirectionConfigFile" ["strategy", "head_chars", "tail_chars"] v
    ToolDirectionConfigFile
      <$> v .:? "strategy"
      <*> v .:? "head_chars"
      <*> v .:? "tail_chars"

instance FromJSON DelegateConfigFile where
  parseJSON = withObject "DelegateConfigFile" $ \v -> do
    rejectUnknownKeys "DelegateConfigFile" ["agent", "extra_agents", "allowed_models"] v
    DelegateConfigFile
      <$> v .:? "agent" .!= AgentPreset Nothing mempty
      <*> v .:? "extra_agents" .!= Map.empty
      <*> v .:? "allowed_models"

instance FromJSON MCPServerConfigFile where
  parseJSON = withObject "MCPServerConfigFile" $ \v -> do
    rejectUnknownKeys "MCPServerConfigFile" ["command", "args", "env", "startup_delay"] v
    MCPServerConfigFile
      <$> v .: "command"
      <*> v .:? "args"
      <*> v .:? "env"
      <*> v .:? "startup_delay"

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

  -- Resolve sub-configs via monoid layering
  let compact = resolveCompaction (fromMaybe (CompactionConfigFile Nothing Nothing Nothing Nothing) configFile.compaction)
  let prune = resolvePruning (fromMaybe mempty configFile.pruning)
  let profile = resolveProfile configFile.agentOverrides

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
      resolveDeliveryTarget (DeliveryTargetFileTelegram ids) = TelegramDelivery ids
      resolveDeliveryTarget DeliveryTargetFileBroadcast = TelegramBroadcast
      resolveDeliveryTarget DeliveryTargetFileLog = LogOnly

  let workspaceDir_ = fromMaybe "/var/lib/assistant/workspace" configFile.workspaceDir
      telegramChatFiles = fromMaybe [] $ configFile.channels >>= (.telegram)

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
                        prompt = fromMaybe [] ep.prompt,
                        session = maybe Isolated Named ep.session,
                        deliveryTarget = maybe TelegramBroadcast resolveDeliveryTarget ep.deliveryTarget,
                        suppressIfContains = ep.suppressIfContains,
                        overrides = ep.overrides
                      }
                  | ep <- eps
                  ]
            }

  let telegramChats_ =
        [ TelegramChatConfig
            { id_ = tc.id_,
              session = maybe Isolated Named tc.session,
              overrides = tc.overrides
            }
        | tc <- telegramChatFiles
        ]

  -- Validate no duplicate chat IDs
  let chatIds = map (.id_) telegramChats_
  when (nub chatIds /= chatIds) $
    fail "channels.telegram contains duplicate chat IDs"

  -- Resolve max_image_dimension: absent -> Just 1568, null -> Nothing, value -> Just value
  let maxImgDim = case configFile.maxImageDimension of
        Nothing -> Just 1568
        Just Nothing -> Nothing
        Just (Just n) -> Just n

  -- Validate max_image_dimension >= 50
  case maxImgDim of
    Just n | n < 50 -> fail "max_image_dimension must be >= 50"
    _ -> pure ()

  let delCfg = fromMaybe (DelegateConfigFile (AgentPreset Nothing mempty) Map.empty Nothing) configFile.delegate

  pure
    Config
      { stateDir = fromMaybe "/var/lib/assistant" configFile.stateDir,
        workspaceDir = workspaceDir_,
        telegramToken = telegramToken_,
        anthropicApiKey = anthropicApiKey_,
        telegramChats = telegramChats_,
        agentProfile = profile,
        compaction = compact,
        pruning = prune,
        mcpServers = servers,
        webhook = webhookCfg,
        toolUseMessages = fromMaybe True configFile.toolUseMessages,
        delegateAgent = delCfg.agent,
        delegateExtraAgents = delCfg.extraAgents,
        delegateAllowedModels = fromMaybe [] delCfg.allowedModels,
        maxImageDimension = maxImgDim
      }
