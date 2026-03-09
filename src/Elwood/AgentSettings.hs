module Elwood.AgentSettings
  ( -- * Partial (monoidal) type for layering overrides
    AgentOverrides (..),

    -- * Cache configuration
    CacheOverrides (..),

    -- * Preset wrapper (overrides + description)
    AgentPreset (..),

    -- * Resolved (concrete) type for runtime use
    AgentProfile (..),

    -- * Tool search configuration
    ToolSearchConfig (..),

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
import Data.Text (Text)
import Data.Vector qualified as V
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (CacheTtl (..))
import Elwood.Permissions (PermissionConfig, PermissionConfigFile, resolvePermissions, toPermissionConfigFile)
import Elwood.Positive (Positive)
import Elwood.Prompt (PromptInput (..))
import Elwood.Thinking (ThinkingLevel (..))
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
  { model :: Last Text,
    thinking :: Last ThinkingLevel,
    maxIterations :: Last Positive,
    cache :: Maybe CacheOverrides,
    maxTokens :: Last Positive,
    systemPrompt :: Last [PromptInput],
    toolSearch :: Last ToolSearchConfig,
    permissions :: Maybe PermissionConfigFile
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically AgentOverrides

-- | Resolved agent profile — all fields concrete. Used at runtime.
data AgentProfile = AgentProfile
  { model :: Text,
    thinking :: ThinkingLevel,
    maxIterations :: Positive,
    cache :: Maybe CacheTtl,
    maxTokens :: Positive,
    systemPrompt :: [PromptInput],
    toolSearch :: ToolSearchConfig,
    permissions :: PermissionConfig
  }
  deriving stock (Show, Eq, Generic)

-- | Hardcoded defaults wrapped as overrides (all 'Just').
agentDefaults :: AgentOverrides
agentDefaults =
  AgentOverrides
    { model = Last (Just "claude-sonnet-4-20250514"),
      thinking = Last (Just ThinkingOff),
      maxIterations = Last (Just 20),
      cache = Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl5Min))),
      maxTokens = Last (Just 16384),
      systemPrompt = Last (Just [WorkspaceFile "SOUL.md"]),
      toolSearch = Last (Just ToolSearchDisabled),
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
   in AgentProfile
        { model = fromMaybe "claude-sonnet-4-20250514" (getLast o.model),
          thinking = fromMaybe ThinkingOff (getLast o.thinking),
          maxIterations = fromMaybe 20 (getLast o.maxIterations),
          cache = resolvedCache,
          maxTokens = fromMaybe 16384 (getLast o.maxTokens),
          systemPrompt = fromMaybe [WorkspaceFile "SOUL.md"] (getLast o.systemPrompt),
          toolSearch = fromMaybe ToolSearchDisabled (getLast o.toolSearch),
          permissions = resolvePermissions (fromMaybe mempty o.permissions)
        }

-- | Wrap resolved profile back into overrides (all 'Just').
toOverrides :: AgentProfile -> AgentOverrides
toOverrides s =
  AgentOverrides
    { model = Last (Just s.model),
      thinking = Last (Just s.thinking),
      maxIterations = Last (Just s.maxIterations),
      cache = case s.cache of
        Nothing -> Just (CacheOverrides (Last (Just False)) (Last Nothing))
        Just ttl -> Just (CacheOverrides (Last (Just True)) (Last (Just ttl))),
      maxTokens = Last (Just s.maxTokens),
      systemPrompt = Last (Just s.systemPrompt),
      toolSearch = Last (Just s.toolSearch),
      permissions = Just (toPermissionConfigFile s.permissions)
    }

-- | Keys accepted in agent override objects.
agentOverrideKeys :: [Key]
agentOverrideKeys = ["model", "thinking", "max_iterations", "cache", "max_tokens", "system_prompt", "tool_search", "permissions"]

-- | Parse agent overrides from an Aeson object (shared by 'AgentOverrides' and 'AgentPreset').
parseAgentOverrides :: Object -> Parser AgentOverrides
parseAgentOverrides v =
  AgentOverrides . Last
    <$> v .:? "model"
    <*> (Last <$> v .:? "thinking")
    <*> (Last <$> v .:? "max_iterations")
    <*> v .:? "cache"
    <*> (Last <$> v .:? "max_tokens")
    <*> (Last <$> v .:? "system_prompt")
    <*> (Last <$> v .:? "tool_search")
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
