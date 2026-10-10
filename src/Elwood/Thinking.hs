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
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Elwood.Aeson (enum, recordOptions, rejectUnknownKeys)
import Elwood.Positive (Positive)
import GHC.Generics (Generic, Generically (..))

data ThinkingMode = Adaptive (Maybe ThinkingEffort) (Maybe ThinkingDisplay) | Budget Positive
  deriving stock (Show, Eq)

data ThinkingEffort = EffortLow | EffortMedium | EffortHigh | EffortXhigh | EffortMax
  deriving stock (Show, Eq)

data ThinkingDisplay = DisplayOmitted | DisplaySummarized | DisplayUpdates
  deriving stock (Show, Eq)

instance FromJSON ThinkingEffort where
  parseJSON = enum "thinking effort" (zip ["low", "medium", "high", "xhigh", "max"] [EffortLow, EffortMedium, EffortHigh, EffortXhigh, EffortMax])

instance FromJSON ThinkingDisplay where
  parseJSON = enum "thinking display" [("omitted", DisplayOmitted), ("summarized", DisplaySummarized), ("updates", DisplayUpdates)]

instance FromJSON ThinkingMode where
  parseJSON = withObject "thinking mode" $ \v -> case KeyMap.toList v of
    [("adaptive", Null)] -> pure (Adaptive Nothing Nothing)
    [("adaptive", value)] ->
      withObject
        "adaptive"
        ( \o -> do
            rejectUnknownKeys "adaptive" ["effort", "display"] o
            Adaptive <$> o .:? "effort" <*> o .:? "display"
        )
        value
    [("fixed", value)] ->
      withObject
        "fixed"
        ( \o -> do
            rejectUnknownKeys "fixed" ["budget_tokens"] o
            Budget <$> o .: "budget_tokens"
        )
        value
    _ -> fail "thinking mode requires exactly one of adaptive or fixed"

data ThinkingOverrides = ThinkingOverrides {enable :: Last Bool, mode :: Last ThinkingMode}
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically ThinkingOverrides

instance FromJSON ThinkingOverrides where
  parseJSON = genericParseJSON recordOptions

resolveThinking :: ThinkingOverrides -> Maybe ThinkingMode
resolveThinking o
  | getLast o.enable == Just True = Just (fromMaybe (Adaptive Nothing Nothing) (getLast o.mode))
  | otherwise = Nothing

progressUpdatesEnabled :: Maybe ThinkingMode -> Bool
progressUpdatesEnabled (Just (Adaptive _ (Just DisplayUpdates))) = True
progressUpdatesEnabled _ = False
