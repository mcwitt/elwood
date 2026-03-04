{-# LANGUAGE StrictData #-}

module Elwood.AgentSettings
  ( -- * Partial (monoidal) type for layering overrides
    AgentOverrides (..),

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
import Elwood.Thinking (ThinkingLevel (..))
import GHC.Generics (Generic)

-- | Partial agent settings for layering overrides.
--
-- Right-biased semigroup: @a <> b@ picks @b@'s values when 'Just'.
data AgentOverrides = AgentOverrides
  { model :: Maybe Text,
    thinking :: Maybe ThinkingLevel,
    maxIterations :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

-- | Right-biased: @a <> b@ picks @b@'s values when 'Just'.
instance Semigroup AgentOverrides where
  a <> b =
    AgentOverrides
      { model = b.model <|> a.model,
        thinking = b.thinking <|> a.thinking,
        maxIterations = b.maxIterations <|> a.maxIterations
      }

instance Monoid AgentOverrides where
  mempty = AgentOverrides Nothing Nothing Nothing

-- | Resolved agent settings — all fields concrete. Used at runtime.
data AgentSettings = AgentSettings
  { model :: Text,
    thinking :: ThinkingLevel,
    maxIterations :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Hardcoded defaults wrapped as overrides (all 'Just').
agentDefaults :: AgentOverrides
agentDefaults =
  AgentOverrides
    { model = Just "claude-sonnet-4-20250514",
      thinking = Just ThinkingOff,
      maxIterations = Just 20
    }

-- | Resolve overrides to concrete settings, using hardcoded fallbacks
-- for any remaining 'Nothing' fields.
resolveAgent :: AgentOverrides -> AgentSettings
resolveAgent o =
  AgentSettings
    { model = fromMaybe "claude-sonnet-4-20250514" o.model,
      thinking = fromMaybe ThinkingOff o.thinking,
      maxIterations = fromMaybe 20 o.maxIterations
    }

-- | Wrap resolved settings back into overrides (all 'Just').
toOverrides :: AgentSettings -> AgentOverrides
toOverrides s =
  AgentOverrides
    { model = Just s.model,
      thinking = Just s.thinking,
      maxIterations = Just s.maxIterations
    }

instance FromJSON AgentOverrides where
  parseJSON = withObject "AgentOverrides" $ \v -> do
    rejectUnknownKeys "AgentOverrides" ["model", "thinking", "max_iterations"] v
    AgentOverrides
      <$> v .:? "model"
      <*> v .:? "thinking"
      <*> v .:? "max_iterations"
