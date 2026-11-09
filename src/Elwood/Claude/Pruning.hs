module Elwood.Claude.Pruning
  ( pruneToolResults,
    pruneThinkingBlocks,
    pruneToolInputs,
    pruneTranscript,
    softPrune,
    protectedBoundary,
    PruneHorizons,
    newPruneHorizons,
    getAndUpdateHorizon,
    PrefixContext (..),
    SentPrefix (..),
    nextThinkingCut,
    getAndUpdateThinkingCut,
    dropThinkingBefore,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Types
import Elwood.Config (PruningConfig (..), PruningStrategy (..), ThinkingPruningConfig (..), ToolDirectionConfig (..), ToolPruningConfig (..))

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
pruneThinkingBlocks (Just config) horizon = before config.strategy horizon withoutThinking

-- | The request projection of a transcript: tool pruning, then thinking pruning.
pruneTranscript :: Maybe PruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneTranscript Nothing _ transcript = transcript
pruneTranscript (Just config) horizon transcript =
  pruneThinkingBlocks config.thinking horizon $ case config.tools of
    Nothing -> transcript
    Just tools -> pruneToolInputs tools horizon (pruneToolResults tools horizon transcript)

withoutThinking :: ClaudeMessage -> ClaudeMessage
withoutThinking = \case
  ClaudeMessage Assistant blocks -> ClaudeMessage Assistant (filter keep blocks)
  message -> message
  where
    keep (ThinkingBlock {}) = False
    keep (RedactedThinkingBlock {}) = False
    keep _ = True

-- | Remove thinking blocks from the messages before an index. Removing thinking
-- from the start of the history is the one history change the API's
-- thinking-block prefix check accepts.
dropThinkingBefore :: Int -> [ClaudeMessage] -> [ClaudeMessage]
dropThinkingBefore cut messages = map withoutThinking prefix <> suffix
  where
    (prefix, suffix) = splitAt cut messages

-- | Request fields outside @messages@ that a thinking block is bound to: the
-- @system@ prompt and the @tools@ (with their deferral), plus the model and
-- provider, since thinking blocks are model-bound and providers project
-- messages differently.
data PrefixContext = PrefixContext
  { provider :: Text,
    model :: Text,
    system :: Maybe Text,
    tools :: [ToolSchema],
    toolSearch :: Maybe (Set ToolName)
  }
  deriving stock (Show, Eq)

-- | What a session sent at the start of its latest turn: the context and the
-- projected history without thinking blocks, the raw history length, and the
-- thinking cut that request used.
data SentPrefix = SentPrefix
  { context :: PrefixContext,
    historyLength :: Int,
    projected :: [ClaudeMessage],
    cut :: Int
  }
  deriving stock (Show, Eq)

-- | Every thinking block must be sent after exactly the prefix (system, tools
-- and earlier messages, ignoring thinking) under which the model produced it.
-- A turn's messages are sent unpruned within the turn, so the prefix that the
-- history's thinking blocks were produced under is the previous turn's
-- projected history followed by that turn's raw messages. If the current
-- projection differs in any way (pruning advanced, compaction, a new system
-- prompt or tool set, another model, an unknown previous state after a
-- restart), every thinking block in the history is dropped. The cut never
-- moves backwards within a history, so a dropped block is never sent again.
nextThinkingCut :: Maybe SentPrefix -> PrefixContext -> [ClaudeMessage] -> [ClaudeMessage] -> SentPrefix
nextThinkingCut previous context history projectedHistory = SentPrefix context (length history) current cut
  where
    current = map withoutThinking projectedHistory
    cut = case previous of
      Just sent
        | sent.context == context,
          sent.projected <> map withoutThinking (drop sent.historyLength history) == current ->
            sent.cut
      _ -> length history

data PruneHorizons = PruneHorizons (TVar (Map Text Int)) (TVar (Map Text SentPrefix))

newPruneHorizons :: IO PruneHorizons
newPruneHorizons = PruneHorizons <$> newTVarIO Map.empty <*> newTVarIO Map.empty

getAndUpdateHorizon :: PruneHorizons -> Text -> Int -> Bool -> IO Int
getAndUpdateHorizon (PruneHorizons state _) session count expired = atomically $ do
  horizons <- readTVar state
  let horizon = if expired then count else min count (Map.findWithDefault 0 session horizons)
  writeTVar state (Map.insert session horizon horizons)
  pure horizon

-- | Record the start of a session's turn and return the index before which its
-- requests must drop thinking blocks. See 'nextThinkingCut'.
getAndUpdateThinkingCut :: PruneHorizons -> Text -> PrefixContext -> [ClaudeMessage] -> [ClaudeMessage] -> IO Int
getAndUpdateThinkingCut (PruneHorizons _ state) session context history projectedHistory = atomically $ do
  sent <- readTVar state
  let next = nextThinkingCut (Map.lookup session sent) context history projectedHistory
  writeTVar state (Map.insert session next sent)
  pure next.cut
