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
  parseJSON v = case parseThinkingLevel v of
    Right tl -> pure tl
    Left err -> fail (T.unpack err)

-- | Parse a thinking level from a YAML value
--
-- Supported formats:
--   false            -> ThinkingOff (YAML parses bare "off" as boolean False)
--   {type: off}      -> ThinkingOff
--   {type: adaptive, effort: low}    -> ThinkingAdaptive EffortLow
--   {type: adaptive, effort: medium} -> ThinkingAdaptive EffortMedium
--   {type: adaptive, effort: high}   -> ThinkingAdaptive EffortHigh
--   {type: fixed, budget_tokens: N}  -> ThinkingBudget N
parseThinkingLevel :: Value -> Either Text ThinkingLevel
parseThinkingLevel (Bool False) = Right ThinkingOff
parseThinkingLevel (Object obj) = case lookupText "type" obj of
  Just "adaptive" -> case lookupText "effort" obj of
    Just "low" -> Right (ThinkingAdaptive EffortLow)
    Just "medium" -> Right (ThinkingAdaptive EffortMedium)
    Just "high" -> Right (ThinkingAdaptive EffortHigh)
    Nothing -> Right (ThinkingAdaptive EffortMedium) -- default effort
    Just other -> Left $ "Invalid effort '" <> other <> "'. Allowed: low, medium, high"
  Just "fixed" -> case KM.lookup (Key.fromText "budget_tokens") obj of
    Just (Number n) | n > 0 -> Right (ThinkingBudget (round n))
    Just (Number _) -> Left "budget_tokens must be positive"
    _ -> Left "Fixed thinking requires a positive budget_tokens"
  Just "off" -> Right ThinkingOff
  Just other -> Left $ "Invalid thinking type '" <> other <> "'. Allowed: off, adaptive, fixed"
  Nothing -> Left "Thinking object requires a 'type' field"
parseThinkingLevel _ = Left "Invalid thinking config. Expected: off, {type: off}, {type: adaptive, effort: ...}, or {type: fixed, budget_tokens: ...}"

-- | Look up a text value in a KeyMap, also handling Bool False for "off"
lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (Key.fromText key) obj of
  Just (String t) -> Just (T.toLower t)
  Just (Bool False) -> Just "off"
  _ -> Nothing
