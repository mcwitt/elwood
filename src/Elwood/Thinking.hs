{-# LANGUAGE StrictData #-}

module Elwood.Thinking
  ( ThinkingLevel (..),
    ThinkingEffort (..),
    parseThinkingLevel,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Extended thinking level for Claude
data ThinkingLevel
  = ThinkingOff
  | ThinkingAdaptive ThinkingEffort
  | -- | Explicit budget_tokens (for older models)
    ThinkingBudget Int
  deriving stock (Show, Eq, Generic)

-- | Effort level for adaptive thinking
data ThinkingEffort = EffortLow | EffortMedium | EffortHigh
  deriving stock (Show, Eq, Generic)

instance FromJSON ThinkingLevel where
  parseJSON = pure . parseThinkingLevel

-- | Parse a thinking level from a YAML value
--
-- Supported formats:
--   off / false          -> ThinkingOff (YAML parses bare "off" as boolean False)
--   {type: off}          -> ThinkingOff
--   {type: adaptive, effort: low}    -> ThinkingAdaptive EffortLow
--   {type: adaptive, effort: medium} -> ThinkingAdaptive EffortMedium
--   {type: adaptive, effort: high}   -> ThinkingAdaptive EffortHigh
--   {type: fixed, budgetTokens: N}   -> ThinkingBudget N
parseThinkingLevel :: Value -> ThinkingLevel
parseThinkingLevel (Bool False) = ThinkingOff
parseThinkingLevel (String t)
  | T.toLower t == "off" = ThinkingOff
parseThinkingLevel (Object obj) = case lookupText "type" obj of
  Just "adaptive" -> case lookupText "effort" obj of
    Just "low" -> ThinkingAdaptive EffortLow
    Just "medium" -> ThinkingAdaptive EffortMedium
    Just "high" -> ThinkingAdaptive EffortHigh
    _ -> ThinkingAdaptive EffortMedium -- default effort
  Just "fixed" -> case KM.lookup (Key.fromText "budget_tokens") obj of
    Just (Number n) | n > 0 -> ThinkingBudget (round n)
    _ -> ThinkingOff
  Just "off" -> ThinkingOff
  _ -> ThinkingOff
parseThinkingLevel _ = ThinkingOff

-- | Look up a text value in a KeyMap
lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
  Just (String t) -> Just (T.toLower t)
  _ -> Nothing
