module Elwood.Claude.Pruning
  ( pruneToolResults,
    pruneThinkingBlocks,
    pruneToolInputs,
    softPrune,
    protectedBoundary,
    PruneHorizons,
    newPruneHorizons,
    getAndUpdateHorizon,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Types
import Elwood.Config (PruningStrategy (..), ThinkingPruningConfig (..), ToolDirectionConfig (..), ToolPruningConfig (..))

softPrune :: Int -> Int -> Text -> Maybe Text
softPrune headChars tailChars text
  | toInteger size <= toInteger headChars + toInteger tailChars + 60 = Nothing
  | otherwise = Just (T.take headChars text <> "\n\n... [pruned " <> T.pack (show (size - headChars - tailChars)) <> " of " <> T.pack (show size) <> " characters] ...\n\n" <> T.takeEnd tailChars text)
  where
    size = T.length text

protectedBoundary :: PruningStrategy -> [ClaudeMessage] -> Int
protectedBoundary strategy messages
  | count <= 0 = length messages
  | otherwise = case drop (length boundaries - count) boundaries of
      index : _ -> index
      [] -> 0
  where
    boundaries = turnBoundaryIndices messages
    count = case strategy of
      KeepTurns turns -> fromIntegral (min turns (fromIntegral (length boundaries)))
      KeepFraction fraction -> ceiling (fraction * fromIntegral (length boundaries))

before :: PruningStrategy -> Int -> (ClaudeMessage -> ClaudeMessage) -> [ClaudeMessage] -> [ClaudeMessage]
before strategy horizon transform messages = map transform prefix <> suffix
  where
    (prefix, suffix) = splitAt (max 0 (min horizon (protectedBoundary strategy messages))) messages

pruneToolResults :: ToolPruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolResults config horizon = before config.output.strategy horizon $ \case
  ClaudeMessage User blocks -> ClaudeMessage User (map block blocks)
  message -> message
  where
    block (ToolResultBlock ident parts False) = ToolResultBlock ident (map part parts) False
    block other = other
    part (ToolResultText text) = ToolResultText (fromMaybe text (softPrune config.output.headChars config.output.tailChars text))
    part image = image

pruneToolInputs :: ToolPruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolInputs config horizon = before config.input.strategy horizon $ \case
  ClaudeMessage Assistant blocks -> ClaudeMessage Assistant (map block blocks)
  message -> message
  where
    block original@(ToolUseBlock ident name input) =
      maybe
        original
        (ToolUseBlock ident name . object . pure . ("_pruned" .=))
        (softPrune config.input.headChars config.input.tailChars (decodeUtf8 (LBS.toStrict (encode input))))
    block other = other

pruneThinkingBlocks :: Maybe ThinkingPruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneThinkingBlocks Nothing _ = id
pruneThinkingBlocks (Just config) horizon = before config.strategy horizon $ \case
  ClaudeMessage Assistant blocks -> ClaudeMessage Assistant (filter keep blocks)
  message -> message
  where
    keep (ThinkingBlock {}) = False
    keep (RedactedThinkingBlock {}) = False
    keep _ = True

newtype PruneHorizons = PruneHorizons (TVar (Map Text Int))

newPruneHorizons :: IO PruneHorizons
newPruneHorizons = PruneHorizons <$> newTVarIO Map.empty

getAndUpdateHorizon :: PruneHorizons -> Text -> Int -> Bool -> IO Int
getAndUpdateHorizon (PruneHorizons state) session count expired = atomically $ do
  horizons <- readTVar state
  let horizon = if expired then count else min count (Map.findWithDefault 0 session horizons)
  writeTVar state (Map.insert session horizon horizons)
  pure horizon
