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
    resolvePruning,
    MCPServerConfig (..),
    PermissionConfig (..),
    PermissionConfigFile (..),
    loadConfig,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.AgentSettings
import Elwood.Event.Types (SessionConfig (..))
import Elwood.Permissions (PermissionConfig (..), PermissionConfigFile (..))
import Elwood.Positive (Positive, literal)
import Elwood.Provider
import Elwood.Webhook.Types
import GHC.Generics (Generic)
import Network.HTTP.Client (parseRequest)
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)

data PruningStrategy
  = KeepTurns Natural
  | KeepFraction Double
  deriving stock (Show, Eq, Generic)

data CompactionStrategy
  = CKeepTurns Positive
  | CKeepFraction Double
  deriving stock (Show, Eq, Generic)

data Config = Config
  { stateDir :: FilePath,
    workspace :: FilePath,
    telegramToken :: Text,
    providers :: Map Text ProviderConfig,
    telegramChats :: [TelegramChatConfig],
    agentProfile :: AgentProfile,
    compaction :: Maybe CompactionConfig,
    pruning :: Maybe PruningConfig,
    mcpServers :: [MCPServerConfig],
    webhook :: WebhookServerConfig,
    toolUseMessages :: Bool,
    delegateAgent :: AgentPreset,
    delegateExtraAgents :: Map Text AgentPreset,
    delegateAllowedModels :: [Text],
    maxImageDimension :: Maybe Int
  }
  deriving stock (Show, Generic)

data CompactionConfig = CompactionConfig
  { tokenThreshold :: Positive,
    model :: ModelRef,
    prompt :: Maybe Text,
    strategy :: CompactionStrategy
  }
  deriving stock (Show, Generic)

data PruningConfig = PruningConfig
  { strategy :: PruningStrategy,
    thinking :: Maybe ThinkingPruningConfig,
    tools :: Maybe ToolPruningConfig
  }
  deriving stock (Show, Eq, Generic)

newtype ThinkingPruningConfig = ThinkingPruningConfig
  { strategy :: PruningStrategy
  }
  deriving stock (Show, Eq, Generic)

data ToolPruningConfig = ToolPruningConfig
  { headChars :: Int,
    tailChars :: Int,
    strategy :: PruningStrategy,
    input :: ToolDirectionConfig,
    output :: ToolDirectionConfig
  }
  deriving stock (Show, Eq, Generic)

data ToolDirectionConfig = ToolDirectionConfig
  { strategy :: PruningStrategy,
    headChars :: Int,
    tailChars :: Int
  }
  deriving stock (Show, Eq, Generic)

data MCPServerConfig = MCPServerConfig
  { name :: Text,
    command :: Text,
    args :: [Text],
    env :: Maybe [(Text, Text)],
    startupDelay :: Int
  }
  deriving stock (Show, Generic)

data TelegramChatConfig = TelegramChatConfig
  { id_ :: Int64,
    session :: SessionConfig,
    toolUseMessages :: Maybe Bool,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Eq, Generic)

-- A retention strategy is a single alternative, never two competing knobs.
retention :: (Natural -> a) -> (Double -> a) -> Value -> Parser a
retention turns fraction = withObject "retention" $ \v -> case KeyMap.keys v of
  ["keep_turns"] -> turns <$> v .: "keep_turns"
  ["keep_fraction"] -> do
    n <- v .: "keep_fraction"
    if n > 0 && n <= 1 then pure (fraction n) else fail "keep_fraction must be in (0, 1]"
  _ -> fail "retention requires exactly one of keep_turns or keep_fraction"

instance FromJSON PruningStrategy where
  parseJSON = retention KeepTurns KeepFraction

instance FromJSON CompactionStrategy where
  parseJSON = withObject "compaction retention" $ \v -> case KeyMap.keys v of
    ["keep_turns"] -> CKeepTurns <$> v .: "keep_turns"
    _ -> retention (const (CKeepTurns (literal @1))) CKeepFraction (Object v)

parseCompaction :: Value -> Parser (Maybe CompactionConfig)
parseCompaction = withObject "compaction" $ \v -> do
  rejectUnknownKeys "compaction" ["enable", "token_threshold", "model", "provider", "prompt", "strategy"] v
  enabled <- v .:? "enable" .!= True
  config <-
    CompactionConfig
      <$> v .:? "token_threshold" .!= literal @50000
      <*> (resolveModelRef "claude-3-5-haiku-20241022" <$> parseModelRefOverrides v)
      <*> v .:? "prompt"
      <*> v .:? "strategy" .!= CKeepTurns (literal @10)
  pure (if enabled then Just config else Nothing)

-- Resolve inherited values while parsing, rather than constructing a second
-- hierarchy of optional runtime records. Missing children inherit; null disables.
resolvePruning :: Value -> Parser (Maybe PruningConfig)
resolvePruning = component "pruning" ["strategy", "thinking", "tools"] $ \v -> do
  strategy <- v .:? "strategy" .!= KeepTurns 3
  thinking <-
    component
      "thinking pruning"
      ["strategy"]
      (\o -> ThinkingPruningConfig <$> o .:? "strategy" .!= strategy)
      (child "thinking" v)
  tools <- component "tool pruning" ["head_chars", "tail_chars", "strategy", "input", "output"] (parseTools strategy) (child "tools" v)
  pure (PruningConfig strategy thinking tools)
  where
    child key = fromMaybe (Object mempty) . KeyMap.lookup key
    parseTools inherited v = do
      headChars <- nonnegative 500 v "head_chars"
      tailChars <- nonnegative 500 v "tail_chars"
      strategy <- v .:? "strategy" .!= inherited
      let direction = withObject "pruning direction" $ \o -> do
            rejectUnknownKeys "pruning direction" ["strategy", "head_chars", "tail_chars"] o
            ToolDirectionConfig <$> o .:? "strategy" .!= strategy <*> nonnegative headChars o "head_chars" <*> nonnegative tailChars o "tail_chars"
      ToolPruningConfig headChars tailChars strategy
        <$> (v .:? "input" .!= Object mempty >>= direction)
        <*> (v .:? "output" .!= Object mempty >>= direction)

component :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser (Maybe a)
component name keys parse value = case value of
  Null -> pure Nothing
  _ ->
    withObject
      name
      ( \v -> do
          rejectUnknownKeys name ("enable" : keys) v
          enabled <- v .:? "enable" .!= True
          parsed <- parse v
          pure (if enabled then Just parsed else Nothing)
      )
      value

nonnegative :: Int -> Object -> Key -> Parser Int
nonnegative fallback v key = do
  n <- v .:? key .!= fallback
  when (n < 0) (fail (show key <> " must be nonnegative"))
  pure n

-- Parse the full file before resolving secrets. Optional nested records are
-- interpreted once; no second tree of transport-specific configuration mirrors.
parseConfig :: Value -> Parser (Config, Map Text ProviderConfigFile)
parseConfig = withObject "Config" $ \v -> do
  rejectUnknownKeys "Config" ["state_dir", "workspace", "channels", "agent", "compaction", "pruning", "mcp_servers", "providers", "webhook", "tool_use_messages", "delegate", "max_image_dimension"] v
  stateDir <- v .:? "state_dir" .!= "/var/lib/assistant"
  workspace <- v .:? "workspace" .!= "/var/lib/assistant/workspace"
  channels <- v .:? "channels" .!= mempty
  rejectUnknownKeys "channels" ["telegram"] channels
  chats <- channels .:? "telegram" .!= [] >>= traverse (withObject "telegram chat" parseChat)
  when (nub (map (.id_) chats) /= map (.id_) chats) (fail "channels.telegram contains duplicate chat IDs")
  profile <- resolveProfile <$> v .:? "agent" .!= mempty
  compact <- v .:? "compaction" .!= Object mempty >>= parseCompaction
  pruning <- v .:? "pruning" .!= Object mempty >>= resolvePruning
  servers <- v .:? "mcp_servers" .!= mempty >>= Map.traverseWithKey (withObject "MCP server" . parseServer)
  providers <- v .:? "providers" .!= mempty
  webhookConfig <- v .:? "webhook" .!= WebhookServerConfig False 8080 Nothing []
  unless (webhookConfig.port >= 1 && webhookConfig.port <= 65535) (fail "webhook port must be in [1, 65535]")
  toolMessages <- v .:? "tool_use_messages" .!= False
  delegate <- v .:? "delegate" .!= mempty
  rejectUnknownKeys "delegate" ["agent", "extra_agents", "allowed_models"] delegate
  delegateAgent <- delegate .:? "agent" .!= AgentPreset Nothing mempty
  extraAgents <- delegate .:? "extra_agents" .!= mempty
  allowedModels <- delegate .:? "allowed_models" .!= []
  imageDimension <- v .:! "max_image_dimension" .!= Just 1568
  when (maybe False (< 50) imageDimension) (fail "max_image_dimension must be >= 50")
  pure (Config stateDir workspace "" mempty chats profile compact pruning (Map.elems servers) webhookConfig toolMessages delegateAgent extraAgents allowedModels imageDimension, providers)
  where
    parseChat v = do
      rejectUnknownKeys "telegram chat" ["id", "session", "tool_use_messages", "agent"] v
      TelegramChatConfig <$> v .: "id" <*> (maybe Isolated Named <$> v .:? "session") <*> v .:? "tool_use_messages" <*> v .:? "agent" .!= mempty
    parseServer name v = do
      rejectUnknownKeys "MCP server" ["command", "args", "env", "startup_delay"] v
      MCPServerConfig name <$> v .: "command" <*> v .:? "args" .!= [] <*> (fmap Map.toList <$> v .:? "env") <*> nonnegative 0 v "startup_delay"

loadConfig :: FilePath -> IO Config
loadConfig path = do
  (config, providerFiles) <- Yaml.decodeFileThrow path >>= either fail pure . parseEither parseConfig
  token <- lookupEnv "TELEGRAM_BOT_TOKEN" >>= maybe (fail "TELEGRAM_BOT_TOKEN environment variable is required") (pure . Text.pack)
  anthropicKey <- fmap (Text.pack <$>) (lookupEnv "ANTHROPIC_API_KEY")
  secret <- fmap (Text.pack <$>) (lookupEnv "WEBHOOK_SECRET")
  providers <- Map.traverseWithKey resolveProvider providerFiles
  let builtin = ProviderConfig "anthropic" "https://api.anthropic.com" (anthropicKey >>= nonempty) AnthropicFormat ImagesEmbedded
      resolved = Map.insertWith (\_ existing -> existing) "anthropic" builtin providers
      overrides =
        map (.overrides) config.telegramChats
          <> map (.overrides) config.webhook.webhooks
          <> map (.overrides) (config.delegateAgent : Map.elems config.delegateExtraAgents)
      references =
        Set.fromList $
          config.agentProfile.model.provider
            : maybe [] (\c -> [c.model.provider]) config.compaction
              <> [p | o <- overrides, Just p <- [getLast o.model.provider]]
  forM_ references $ \name -> unless (Map.member name resolved) (fail ("unknown provider: " <> Text.unpack name))
  when (Set.member "anthropic" references && Map.notMember "anthropic" providers && isNothing builtin.apiKey) $
    fail "ANTHROPIC_API_KEY is required because the built-in 'anthropic' provider is used"
  let w = config.webhook
  pure config {telegramToken = token, providers = resolved, webhook = WebhookServerConfig w.enabled w.port (secret <|> w.secret) w.webhooks}
  where
    nonempty t = if Text.null t then Nothing else Just t
    resolveProvider name p = do
      when (Text.null (Text.strip p.baseUrl)) (fail "base_url must not be empty")
      unless (any (`Text.isPrefixOf` p.baseUrl) ["https://", "http://"]) (fail "provider base_url must be HTTP(S)")
      _ <- parseRequest (Text.unpack p.baseUrl)
      when (isJust p.apiKey && isJust p.apiKeyEnv) (fail "set either api_key or api_key_env, not both")
      key <- case p.apiKeyEnv of
        Nothing -> pure p.apiKey
        Just variable -> lookupEnv (Text.unpack variable) >>= maybe (fail ("api_key_env is not set: " <> Text.unpack variable)) (pure . nonempty . Text.pack)
      when (isJust p.apiKeyEnv && isNothing key) (fail "api_key_env is empty")
      pure (ProviderConfig name p.baseUrl key (fromMaybe AnthropicFormat p.format) (fromMaybe ImagesEmbedded p.toolResultImages))
