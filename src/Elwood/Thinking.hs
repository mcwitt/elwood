module Elwood.Thinking
  ( ThinkingMode (..),
    ThinkingEffort (..),
    ThinkingOverrides (..),
    resolveThinking,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text qualified as T
import Elwood.Aeson (rejectUnknownKeys)
import GHC.Generics (Generic, Generically (..))

-- | Active thinking mode (no "off" variant — use 'Maybe ThinkingMode')
data ThinkingMode
  = Adaptive (Maybe ThinkingEffort)
  | Budget Int
  deriving stock (Show, Eq, Generic)

-- | Effort level for adaptive thinking
data ThinkingEffort = EffortLow | EffortMedium | EffortHigh
  deriving stock (Show, Eq, Generic)

instance FromJSON ThinkingEffort where
  parseJSON = withText "ThinkingEffort" $ \t ->
    case T.toLower t of
      "low" -> pure EffortLow
      "medium" -> pure EffortMedium
      "high" -> pure EffortHigh
      _ -> fail $ "Invalid effort '" <> T.unpack t <> "'. Allowed: low, medium, high"

-- | Parse a 'ThinkingMode' from a single-key object (attrTag-style YAML):
--
--   @{adaptive: {}}@                     → @Adaptive Nothing@
--   @{adaptive: {effort: low}}@          → @Adaptive (Just EffortLow)@
--   @{fixed: {budget_tokens: 4096}}@     → @Budget 4096@
instance FromJSON ThinkingMode where
  parseJSON = withObject "ThinkingMode" $ \obj ->
    case KM.toList obj of
      [(k, v)]
        | k == Key.fromText "adaptive" -> parseAdaptive v
        | k == Key.fromText "fixed" -> parseFixed v
        | otherwise -> fail $ "Unknown thinking mode '" <> show k <> "'. Allowed: adaptive, fixed"
      _ -> fail "ThinkingMode must be a single-key object like {adaptive: {}} or {fixed: {budget_tokens: 4096}}"
    where
      parseAdaptive v = case v of
        Object inner -> Adaptive <$> inner .:? "effort"
        Null -> pure (Adaptive Nothing)
        _ -> fail "adaptive value must be an object or null"
      parseFixed = withObject "fixed" $ \inner -> do
        n <- inner .: "budget_tokens"
        if n > 0
          then pure (Budget n)
          else fail "budget_tokens must be positive"

-- | Layered thinking configuration for merging overrides.
data ThinkingOverrides = ThinkingOverrides
  { enable :: Last Bool,
    mode :: Last ThinkingMode
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically ThinkingOverrides

instance FromJSON ThinkingOverrides where
  parseJSON = withObject "ThinkingOverrides" $ \v -> do
    rejectUnknownKeys "ThinkingOverrides" ["enable", "mode"] v
    ThinkingOverrides . Last <$> v .:? "enable" <*> (Last <$> v .:? "mode")

-- | Resolve thinking overrides to a concrete mode.
--
-- @enable = false@ or absent → 'Nothing'
-- @enable = true@ + mode → @Just mode@ (default: @Adaptive Nothing@)
resolveThinking :: ThinkingOverrides -> Maybe ThinkingMode
resolveThinking o =
  case getLast o.enable of
    Just True -> Just (fromMaybe (Adaptive Nothing) (getLast o.mode))
    _ -> Nothing
