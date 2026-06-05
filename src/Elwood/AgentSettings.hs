module Elwood.AgentSettings
  ( -- * Model reference
    ModelRef (..),
    ModelRefOverrides (..),
    resolveModelRef,
    parseModelRefOverrides,

    -- * Partial (monoidal) type for layering overrides
    AgentOverrides (..),

    -- * Cache configuration
    CacheOverrides (..),

    -- * Preset wrapper (overrides + description)
    AgentPreset (..),

    -- * Resolved (concrete) type for runtime use
    AgentProfile (..),

    -- * Tool search configuration
    ToolSearchConfig (..),

    -- * Tool availability filter
    ToolFilter (..),

    -- * Defaults and resolution
    agentDefaults,
    resolveProfile,
    toOverrides,
  )
where

import Data.Aeson (FromJSON (..), Key, Object, Value (..), withObject, (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (CacheTtl (..), ToolName (..))
import Elwood.Permissions (PermissionConfig, PermissionConfigFile, resolvePermissions, toPermissionConfigFile)
import Elwood.Positive (Positive)
import Elwood.Prompt (PromptInput (..))
import Elwood.Thinking (ThinkingMode (..), ThinkingOverrides (..), resolveThinking)
import GHC.Generics (Generic, Generically (..))

-- | Tool search configuration
--
-- Supported YAML formats:
--   @false@ / absent → 'ToolSearchDisabled'
--   @true@ / @[]@    → @'ToolSearchEnabled' []@ (all tools deferred)
--   @[tool1, tool2]@ → @'ToolSearchEnabled' [tool1, tool2]@ (listed tools never deferred)
data ToolSearchConfig
  = ToolSearchDisabled
  | ToolSearchEnabled [Text]
  deriving stock (Show, Eq, Generic)

instance FromJSON ToolSearchConfig where
  parseJSON (Bool False) = pure ToolSearchDisabled
  parseJSON (Bool True) = pure (ToolSearchEnabled [])
  parseJSON (Array arr) = pure $ ToolSearchEnabled [t | String t <- V.toList arr]
  parseJSON _ = fail "tool_search must be false, true, or an array of tool names"

-- | Which tools are available (advertised to the model and executable) for an
-- agent. Orthogonal to 'ToolSearchConfig', which only controls deferral.
--
-- Supported YAML formats:
--   absent / @all@   → 'AllTools'
--   @[a, b, ...]@    → @'OnlyTools' {a, b, ...}@   (@[]@ ⇒ no tools)
--
-- The @all@ keyword exists for layering: a higher override layer can re-open a
-- restrictive preset by setting @tools: all@.
data ToolFilter
  = AllTools
  | OnlyTools (Set ToolName)
  deriving stock (Show, Eq, Generic)

instance FromJSON ToolFilter where
  parseJSON (String "all") = pure AllTools
  parseJSON (Array arr) = pure $ OnlyTools (Set.fromList [ToolName t | String t <- V.toList arr])
  parseJSON _ = fail "tools must be \"all\" or an array of tool names"

-- | A model reference: which provider, and which model on it.
-- Resolved form (total).
data ModelRef = ModelRef
  { provider :: Text,
    model :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Partial / layerable model reference. 'provider' and 'model' layer
-- independently (right-biased), so "change the model, keep the provider"
-- works; but they are bound as one record so no config site accepts one
-- without the other.
data ModelRefOverrides = ModelRefOverrides
  { provider :: Last Text,
    model :: Last Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically ModelRefOverrides

-- | Resolve a model reference against a default model. 'provider' defaults to
-- "anthropic". The default model is a parameter because callers differ
-- (agent => sonnet, compaction => haiku).
resolveModelRef :: Text -> ModelRefOverrides -> ModelRef
resolveModelRef defaultModel o =
  ModelRef
    { provider = fromMaybe "anthropic" (getLast o.provider),
      model = fromMaybe defaultModel (getLast o.model)
    }

-- | Partial cache configuration for layering overrides.
-- Right-biased field-level merge (like 'PermissionConfigFile').
data CacheOverrides = CacheOverrides
  { enable :: Last Bool,
    ttl :: Last CacheTtl
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically CacheOverrides

instance FromJSON CacheOverrides where
  parseJSON = withObject "CacheOverrides" $ \v -> do
    rejectUnknownKeys "CacheOverrides" ["enable", "ttl"] v
    CacheOverrides . Last <$> v .:? "enable" <*> (Last <$> v .:? "ttl")

-- | Partial agent settings for layering overrides.
--
-- 'Last' fields are right-biased replace; 'Maybe' fields deep-merge via their 'Semigroup'.
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

-- | Resolved agent profile — all fields concrete. Used at runtime.
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
  deriving stock (Show, Eq, Generic)

-- | Hardcoded defaults wrapped as overrides (all 'Just').
agentDefaults :: AgentOverrides
agentDefaults =
  AgentOverrides
    { model = ModelRefOverrides (Last (Just "anthropic")) (Last (Just "claude-sonnet-4-20250514")),
      thinking = Just (ThinkingOverrides (Last (Just False)) (Last Nothing)),
      maxIterations = Last (Just 20),
      cache = Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl5Min))),
      maxTokens = Last (Just 16384),
      systemPrompt = Last (Just [WorkspaceFile "SOUL.md"]),
      toolSearch = Last (Just ToolSearchDisabled),
      toolFilter = Last (Just AllTools),
      permissions = Just mempty
    }

-- | Resolve overrides to a concrete profile against hardcoded defaults.
resolveProfile :: AgentOverrides -> AgentProfile
resolveProfile o =
  let resolvedCache = case o.cache of
        Just co
          | co.enable == Last (Just False) -> Nothing
          | otherwise -> Just (fromMaybe CacheTtl5Min (getLast co.ttl))
        Nothing -> Just CacheTtl5Min
      resolvedThinking = resolveThinking (fromMaybe mempty o.thinking)
   in AgentProfile
        { model = resolveModelRef "claude-sonnet-4-20250514" o.model,
          thinking = resolvedThinking,
          maxIterations = fromMaybe 20 (getLast o.maxIterations),
          cache = resolvedCache,
          maxTokens = fromMaybe 16384 (getLast o.maxTokens),
          systemPrompt = fromMaybe [WorkspaceFile "SOUL.md"] (getLast o.systemPrompt),
          toolSearch = fromMaybe ToolSearchDisabled (getLast o.toolSearch),
          toolFilter = fromMaybe AllTools (getLast o.toolFilter),
          permissions = resolvePermissions (fromMaybe mempty o.permissions)
        }

-- | Wrap resolved profile back into overrides (all 'Just').
toOverrides :: AgentProfile -> AgentOverrides
toOverrides s =
  AgentOverrides
    { model = ModelRefOverrides (Last (Just s.model.provider)) (Last (Just s.model.model)),
      thinking = Just $ case s.thinking of
        Nothing -> ThinkingOverrides (Last (Just False)) (Last Nothing)
        Just m -> ThinkingOverrides (Last (Just True)) (Last (Just m)),
      maxIterations = Last (Just s.maxIterations),
      cache = case s.cache of
        Nothing -> Just (CacheOverrides (Last (Just False)) (Last Nothing))
        Just ttl -> Just (CacheOverrides (Last (Just True)) (Last (Just ttl))),
      maxTokens = Last (Just s.maxTokens),
      systemPrompt = Last (Just s.systemPrompt),
      toolSearch = Last (Just s.toolSearch),
      toolFilter = Last (Just s.toolFilter),
      permissions = Just (toPermissionConfigFile s.permissions)
    }

-- | Keys accepted in agent override objects.
agentOverrideKeys :: [Key]
agentOverrideKeys = ["model", "provider", "thinking", "max_iterations", "cache", "max_tokens", "system_prompt", "tool_search", "tools", "permissions"]

-- | Parse a 'ModelRefOverrides' from an object's @provider@ and @model@ keys.
parseModelRefOverrides :: Object -> Parser ModelRefOverrides
parseModelRefOverrides v =
  ModelRefOverrides . Last <$> v .:? "provider" <*> (Last <$> v .:? "model")

-- | Parse agent overrides from an Aeson object (shared by 'AgentOverrides' and 'AgentPreset').
parseAgentOverrides :: Object -> Parser AgentOverrides
parseAgentOverrides v =
  AgentOverrides
    <$> parseModelRefOverrides v
    <*> v .:? "thinking"
    <*> (Last <$> v .:? "max_iterations")
    <*> v .:? "cache"
    <*> (Last <$> v .:? "max_tokens")
    <*> (Last <$> v .:? "system_prompt")
    <*> (Last <$> v .:? "tool_search")
    <*> (Last <$> v .:? "tools")
    <*> v .:? "permissions"

instance FromJSON AgentOverrides where
  parseJSON = withObject "AgentOverrides" $ \v -> do
    rejectUnknownKeys "AgentOverrides" agentOverrideKeys v
    parseAgentOverrides v

-- | Agent preset: overrides plus an optional description.
-- Used in delegate config to document what each preset is for.
data AgentPreset = AgentPreset
  { description :: Maybe Text,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AgentPreset where
  parseJSON = withObject "AgentPreset" $ \v -> do
    rejectUnknownKeys "AgentPreset" ("description" : agentOverrideKeys) v
    AgentPreset <$> v .:? "description" <*> parseAgentOverrides v
