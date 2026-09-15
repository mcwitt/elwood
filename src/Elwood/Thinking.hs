module Elwood.Thinking
  ( ThinkingMode (..),
    ThinkingEffort (..),
    ThinkingDisplay (..),
    ThinkingOverrides (..),
    resolveThinking,
    progressUpdatesEnabled,
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
  = -- | Adaptive thinking with optional effort level and thinking display mode
    Adaptive (Maybe ThinkingEffort) (Maybe ThinkingDisplay)
  | Budget Int
  deriving stock (Show, Eq, Generic)

-- | Effort level for adaptive thinking.
--
-- Note: @EffortXhigh@ is Opus 4.7+ only (and is the recommended default for
-- coding/agentic use cases on that model). @EffortMax@ is Opus-tier only
-- (Opus 4.6+; not supported on Sonnet or Haiku).
data ThinkingEffort
  = EffortLow
  | EffortMedium
  | EffortHigh
  | EffortXhigh
  | EffortMax
  deriving stock (Show, Eq, Generic)

instance FromJSON ThinkingEffort where
  parseJSON = withText "ThinkingEffort" $ \t ->
    case T.toLower t of
      "low" -> pure EffortLow
      "medium" -> pure EffortMedium
      "high" -> pure EffortHigh
      "xhigh" -> pure EffortXhigh
      "max" -> pure EffortMax
      _ -> fail $ "Invalid effort '" <> T.unpack t <> "'. Allowed: low, medium, high, xhigh, max"

-- | What the API returns in @thinking@ blocks (the @thinking.display@
-- request field). Only meaningful with adaptive thinking.
--
-- On Fable-class models the text the model writes between tool calls is
-- returned as progress-update thinking blocks rather than text blocks, so
-- under 'DisplayOmitted' (the API default) that narration is empty and
-- never reaches the user; 'DisplayUpdates' returns it as thinking-block
-- text, which the agent loop then delivers like intermediate text.
data ThinkingDisplay
  = -- | Thinking blocks come back with empty text (API default)
    DisplayOmitted
  | -- | Reasoning summaries and progress updates, mixed
    DisplaySummarized
  | -- | Progress updates only; reasoning stays hidden (beta; Fable 5+)
    DisplayUpdates
  deriving stock (Show, Eq, Generic)

instance FromJSON ThinkingDisplay where
  parseJSON = withText "ThinkingDisplay" $ \t ->
    case T.toLower t of
      "omitted" -> pure DisplayOmitted
      "summarized" -> pure DisplaySummarized
      "updates" -> pure DisplayUpdates
      _ -> fail $ "Invalid display '" <> T.unpack t <> "'. Allowed: omitted, summarized, updates"

-- | Whether a thinking mode asks the API for progress updates
-- ('DisplayUpdates'), in which case non-empty thinking blocks in a
-- response are user-facing narration rather than reasoning.
progressUpdatesEnabled :: Maybe ThinkingMode -> Bool
progressUpdatesEnabled (Just (Adaptive _ (Just DisplayUpdates))) = True
progressUpdatesEnabled _ = False

-- | Parse a 'ThinkingMode' from a single-key object (attrTag-style YAML):
--
--   @{adaptive: {}}@                          → @Adaptive Nothing Nothing@
--   @{adaptive: {effort: low}}@               → @Adaptive (Just EffortLow) Nothing@
--   @{adaptive: {display: updates}}@          → @Adaptive Nothing (Just DisplayUpdates)@
--   @{fixed: {budget_tokens: 4096}}@          → @Budget 4096@
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
        Object inner -> do
          rejectUnknownKeys "adaptive" ["effort", "display"] inner
          Adaptive <$> inner .:? "effort" <*> inner .:? "display"
        Null -> pure (Adaptive Nothing Nothing)
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
-- @enable = true@ + mode → @Just mode@ (default: @Adaptive Nothing Nothing@)
resolveThinking :: ThinkingOverrides -> Maybe ThinkingMode
resolveThinking o =
  case getLast o.enable of
    Just True -> Just (fromMaybe (Adaptive Nothing Nothing) (getLast o.mode))
    _ -> Nothing
