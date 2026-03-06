module Elwood.AgentSettings
  ( -- * Partial (monoidal) type for layering overrides
    AgentOverrides (..),

    -- * Preset wrapper (overrides + description)
    AgentPreset (..),

    -- * Resolved (concrete) type for runtime use
    AgentSettings (..),

    -- * Defaults and resolution
    agentDefaults,
    resolveAgent,
    toOverrides,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject, (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (CacheTtl (..))
import Elwood.Positive (Positive, unsafePositive)
import Elwood.Thinking (ThinkingLevel (..))
import GHC.Generics (Generic)

-- | Partial agent settings for layering overrides.
--
-- Right-biased semigroup: @a <> b@ picks @b@'s values when 'Just'.
data AgentOverrides = AgentOverrides
  { model :: Maybe Text,
    thinking :: Maybe ThinkingLevel,
    maxIterations :: Maybe Positive,
    cacheTtl :: Maybe CacheTtl,
    maxTokens :: Maybe Positive
  }
  deriving stock (Show, Eq, Generic)

-- | Right-biased: @a <> b@ picks @b@'s values when 'Just'.
instance Semigroup AgentOverrides where
  a <> b =
    AgentOverrides
      { model = b.model <|> a.model,
        thinking = b.thinking <|> a.thinking,
        maxIterations = b.maxIterations <|> a.maxIterations,
        cacheTtl = b.cacheTtl <|> a.cacheTtl,
        maxTokens = b.maxTokens <|> a.maxTokens
      }

instance Monoid AgentOverrides where
  mempty = AgentOverrides Nothing Nothing Nothing Nothing Nothing

-- | Resolved agent settings — all fields concrete. Used at runtime.
data AgentSettings = AgentSettings
  { model :: Text,
    thinking :: ThinkingLevel,
    maxIterations :: Positive,
    cacheTtl :: CacheTtl,
    maxTokens :: Positive
  }
  deriving stock (Show, Eq, Generic)

-- | Hardcoded defaults wrapped as overrides (all 'Just').
agentDefaults :: AgentOverrides
agentDefaults =
  AgentOverrides
    { model = Just "claude-sonnet-4-20250514",
      thinking = Just ThinkingOff,
      maxIterations = Just (unsafePositive 20),
      cacheTtl = Just CacheTtl5Min,
      maxTokens = Just (unsafePositive 16384)
    }

-- | Resolve overrides to concrete settings, using hardcoded fallbacks
-- for any remaining 'Nothing' fields.
resolveAgent :: AgentOverrides -> AgentSettings
resolveAgent o =
  AgentSettings
    { model = fromMaybe "claude-sonnet-4-20250514" o.model,
      thinking = fromMaybe ThinkingOff o.thinking,
      maxIterations = fromMaybe (unsafePositive 20) o.maxIterations,
      cacheTtl = fromMaybe CacheTtl5Min o.cacheTtl,
      maxTokens = fromMaybe (unsafePositive 16384) o.maxTokens
    }

-- | Wrap resolved settings back into overrides (all 'Just').
toOverrides :: AgentSettings -> AgentOverrides
toOverrides s =
  AgentOverrides
    { model = Just s.model,
      thinking = Just s.thinking,
      maxIterations = Just s.maxIterations,
      cacheTtl = Just s.cacheTtl,
      maxTokens = Just s.maxTokens
    }

instance FromJSON AgentOverrides where
  parseJSON = withObject "AgentOverrides" $ \v -> do
    rejectUnknownKeys "AgentOverrides" ["model", "thinking", "max_iterations", "cache_ttl", "max_tokens"] v
    AgentOverrides
      <$> v .:? "model"
      <*> v .:? "thinking"
      <*> v .:? "max_iterations"
      <*> v .:? "cache_ttl"
      <*> v .:? "max_tokens"

-- | Agent preset: overrides plus an optional description.
-- Used in delegate config to document what each preset is for.
data AgentPreset = AgentPreset
  { description :: Maybe Text,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AgentPreset where
  parseJSON = withObject "AgentPreset" $ \v -> do
    rejectUnknownKeys "AgentPreset" ["description", "model", "thinking", "max_iterations", "cache_ttl", "max_tokens"] v
    AgentPreset
      <$> v .:? "description"
      <*> ( AgentOverrides
              <$> v .:? "model"
              <*> v .:? "thinking"
              <*> v .:? "max_iterations"
              <*> v .:? "cache_ttl"
              <*> v .:? "max_tokens"
          )
