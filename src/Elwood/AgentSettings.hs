module Elwood.AgentSettings
  ( ModelRef (..),
    ModelRefOverrides (..),
    resolveModelRef,
    parseModelRefOverrides,
    AgentOverrides (..),
    CacheOverrides (..),
    AgentPreset (..),
    AgentProfile (..),
    ToolSearchConfig (..),
    ToolFilter (..),
    agentDefaults,
    resolveProfile,
    toOverrides,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Last (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Elwood.Aeson (field, recordOptions, rejectUnknownKeys)
import Elwood.Claude.Types (CacheTtl (..), ToolName)
import Elwood.Permissions (PermissionConfig, PermissionConfigFile, resolvePermissions, toPermissionConfigFile)
import Elwood.Positive (Positive, literal)
import Elwood.Prompt (PromptInput (..))
import Elwood.Thinking (ThinkingMode, ThinkingOverrides (..), resolveThinking)
import GHC.Generics (Generic, Generically (..))

data ToolSearchConfig = ToolSearchDisabled | ToolSearchEnabled [Text]
  deriving stock (Show, Eq)

instance FromJSON ToolSearchConfig where
  parseJSON (Bool False) = pure ToolSearchDisabled
  parseJSON (Bool True) = pure (ToolSearchEnabled [])
  parseJSON value = ToolSearchEnabled <$> parseJSON value

data ToolFilter = AllTools | OnlyTools (Set ToolName)
  deriving stock (Show, Eq)

instance FromJSON ToolFilter where
  parseJSON (String "all") = pure AllTools
  parseJSON value = OnlyTools . Set.fromList <$> parseJSON value

data ModelRef = ModelRef {provider :: Text, model :: Text}
  deriving stock (Show, Eq)

data ModelRefOverrides = ModelRefOverrides {provider :: Last Text, model :: Last Text}
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically ModelRefOverrides

resolveModelRef :: Text -> ModelRefOverrides -> ModelRef
resolveModelRef model o = ModelRef (fromMaybe "anthropic" (getLast o.provider)) (fromMaybe model (getLast o.model))

parseModelRefOverrides :: Object -> Parser ModelRefOverrides
parseModelRefOverrides v = ModelRefOverrides <$> field v "provider" <*> field v "model"

data CacheOverrides = CacheOverrides {enable :: Last Bool, ttl :: Last CacheTtl}
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically CacheOverrides

instance FromJSON CacheOverrides where
  parseJSON = genericParseJSON recordOptions

-- | Overrides form a right-biased monoid, with nested records composed fieldwise.
data AgentOverrides = AgentOverrides
  { model :: ModelRefOverrides,
    thinking :: Maybe ThinkingOverrides,
    maxIterations :: Last Positive,
    cache :: Maybe CacheOverrides,
    maxTokens :: Last Positive,
    systemPrompt :: Last [PromptInput],
    toolSearch :: Last ToolSearchConfig,
    toolFilter :: Last ToolFilter,
    permissions :: Maybe PermissionConfigFile
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically AgentOverrides

data AgentProfile = AgentProfile
  { model :: ModelRef,
    thinking :: Maybe ThinkingMode,
    maxIterations :: Positive,
    cache :: Maybe CacheTtl,
    maxTokens :: Positive,
    systemPrompt :: [PromptInput],
    toolSearch :: ToolSearchConfig,
    toolFilter :: ToolFilter,
    permissions :: PermissionConfig
  }
  deriving stock (Show, Eq)

agentDefaults :: AgentOverrides
agentDefaults = toOverrides (resolveProfile mempty)

resolveProfile :: AgentOverrides -> AgentProfile
resolveProfile o =
  AgentProfile
    { model = resolveModelRef "claude-sonnet-4-20250514" o.model,
      thinking = resolveThinking (fromMaybe mempty o.thinking),
      maxIterations = fromMaybe (literal @20) (getLast o.maxIterations),
      cache = case fromMaybe mempty o.cache of
        c
          | getLast c.enable == Just False -> Nothing
          | otherwise -> Just (fromMaybe CacheTtl5Min (getLast c.ttl)),
      maxTokens = fromMaybe (literal @16384) (getLast o.maxTokens),
      systemPrompt = fromMaybe [WorkspaceFile "SOUL.md"] (getLast o.systemPrompt),
      toolSearch = fromMaybe ToolSearchDisabled (getLast o.toolSearch),
      toolFilter = fromMaybe AllTools (getLast o.toolFilter),
      permissions = resolvePermissions (fromMaybe mempty o.permissions)
    }

toOverrides :: AgentProfile -> AgentOverrides
toOverrides s =
  AgentOverrides
    (ModelRefOverrides (present s.model.provider) (present s.model.model))
    (Just (ThinkingOverrides (present (isJust s.thinking)) (Last s.thinking)))
    (present s.maxIterations)
    (Just (CacheOverrides (present (isJust s.cache)) (Last s.cache)))
    (present s.maxTokens)
    (present s.systemPrompt)
    (present s.toolSearch)
    (present s.toolFilter)
    (Just (toPermissionConfigFile s.permissions))
  where
    present = Last . Just

parseOverrides :: [Key] -> Object -> Parser AgentOverrides
parseOverrides extra v = do
  rejectUnknownKeys "AgentOverrides" (extra <> ["model", "provider", "thinking", "max_iterations", "cache", "max_tokens", "system_prompt", "tool_search", "tools", "permissions"]) v
  AgentOverrides
    <$> parseModelRefOverrides v
    <*> v .:? "thinking"
    <*> field v "max_iterations"
    <*> v .:? "cache"
    <*> field v "max_tokens"
    <*> field v "system_prompt"
    <*> field v "tool_search"
    <*> field v "tools"
    <*> v .:? "permissions"

instance FromJSON AgentOverrides where
  parseJSON = withObject "AgentOverrides" (parseOverrides [])

data AgentPreset = AgentPreset {description :: Maybe Text, overrides :: AgentOverrides}
  deriving stock (Show, Eq)

instance FromJSON AgentPreset where
  parseJSON = withObject "AgentPreset" $ \v -> AgentPreset <$> v .:? "description" <*> parseOverrides ["description"] v
