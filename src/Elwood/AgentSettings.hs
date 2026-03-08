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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Key, Object, Value (..), withObject, (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as V
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (CacheTtl (..))
import Elwood.Permissions (PermissionConfig, PermissionConfigFile, resolvePermissions, toPermissionConfigFile)
import Elwood.Positive (Positive)
import Elwood.Prompt (PromptInput (..))
import Elwood.Thinking (ThinkingLevel (..))
import GHC.Generics (Generic)

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
  { enable :: Maybe Bool,
    ttl :: Maybe CacheTtl
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup CacheOverrides where
  a <> b = CacheOverrides {enable = b.enable <|> a.enable, ttl = b.ttl <|> a.ttl}

instance FromJSON CacheOverrides where
  parseJSON = withObject "CacheOverrides" $ \v -> do
    rejectUnknownKeys "CacheOverrides" ["enable", "ttl"] v
    CacheOverrides <$> v .:? "enable" <*> v .:? "ttl"

-- | Partial agent settings for layering overrides.
--
-- Right-biased semigroup: @a <> b@ picks @b@'s values when 'Just'.
-- Permissions use field-level merge via 'PermissionConfigFile' 'Semigroup'.
data AgentOverrides = AgentOverrides
  { model :: Maybe Text,
    thinking :: Maybe ThinkingLevel,
    maxIterations :: Maybe Positive,
    cache :: Maybe CacheOverrides,
    maxTokens :: Maybe Positive,
    systemPrompt :: Maybe [PromptInput],
    toolSearch :: Maybe ToolSearchConfig,
    permissions :: Maybe PermissionConfigFile
  }
  deriving stock (Show, Eq, Generic)

-- | Right-biased: @a <> b@ picks @b@'s values when 'Just'.
-- Permissions use field-level merge (not replace) via 'PermissionConfigFile' 'Semigroup'.
instance Semigroup AgentOverrides where
  a <> b =
    AgentOverrides
      { model = b.model <|> a.model,
        thinking = b.thinking <|> a.thinking,
        maxIterations = b.maxIterations <|> a.maxIterations,
        cache = case (a.cache, b.cache) of
          (Nothing, Nothing) -> Nothing
          (Just ca, Nothing) -> Just ca
          (Nothing, Just cb) -> Just cb
          (Just ca, Just cb) -> Just (ca <> cb),
        maxTokens = b.maxTokens <|> a.maxTokens,
        systemPrompt = b.systemPrompt <|> a.systemPrompt,
        toolSearch = b.toolSearch <|> a.toolSearch,
        permissions = case (a.permissions, b.permissions) of
          (Nothing, Nothing) -> Nothing
          (Just pa, Nothing) -> Just pa
          (Nothing, Just pb) -> Just pb
          (Just pa, Just pb) -> Just (pa <> pb)
      }

instance Monoid AgentOverrides where
  mempty = AgentOverrides Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
    { model = Just "claude-sonnet-4-20250514",
      thinking = Just ThinkingOff,
      maxIterations = Just 20,
      cache = Just (CacheOverrides (Just True) (Just CacheTtl5Min)),
      maxTokens = Just 16384,
      systemPrompt = Just [WorkspaceFile "SOUL.md"],
      toolSearch = Just ToolSearchDisabled,
      permissions = Just mempty
    }

-- | Resolve overrides to concrete profile by layering over 'agentDefaults'.
resolveProfile :: AgentOverrides -> AgentProfile
resolveProfile o =
  let d = agentDefaults <> o
      resolvedCache = case d.cache of
        Just co
          | co.enable == Just False -> Nothing
          | otherwise -> Just (fromMaybe CacheTtl5Min co.ttl)
        Nothing -> Just CacheTtl5Min
   in AgentProfile
        { model = fromMaybe "claude-sonnet-4-20250514" d.model,
          thinking = fromMaybe ThinkingOff d.thinking,
          maxIterations = fromMaybe 20 d.maxIterations,
          cache = resolvedCache,
          maxTokens = fromMaybe 16384 d.maxTokens,
          systemPrompt = fromMaybe [WorkspaceFile "SOUL.md"] d.systemPrompt,
          toolSearch = fromMaybe ToolSearchDisabled d.toolSearch,
          permissions = resolvePermissions (fromMaybe mempty d.permissions)
        }

-- | Wrap resolved profile back into overrides (all 'Just').
toOverrides :: AgentProfile -> AgentOverrides
toOverrides s =
  AgentOverrides
    { model = Just s.model,
      thinking = Just s.thinking,
      maxIterations = Just s.maxIterations,
      cache = case s.cache of
        Nothing -> Just (CacheOverrides (Just False) Nothing)
        Just ttl -> Just (CacheOverrides (Just True) (Just ttl)),
      maxTokens = Just s.maxTokens,
      systemPrompt = Just s.systemPrompt,
      toolSearch = Just s.toolSearch,
      permissions = Just (toPermissionConfigFile s.permissions)
    }

-- | Keys accepted in agent override objects.
agentOverrideKeys :: [Key]
agentOverrideKeys = ["model", "thinking", "max_iterations", "cache", "max_tokens", "system_prompt", "tool_search", "permissions"]

-- | Parse agent overrides from an Aeson object (shared by 'AgentOverrides' and 'AgentPreset').
parseAgentOverrides :: Object -> Parser AgentOverrides
parseAgentOverrides v =
  AgentOverrides
    <$> v .:? "model"
    <*> v .:? "thinking"
    <*> v .:? "max_iterations"
    <*> v .:? "cache"
    <*> v .:? "max_tokens"
    <*> v .:? "system_prompt"
    <*> v .:? "tool_search"
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
