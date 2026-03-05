module Elwood.Claude.Pruning
  ( -- * Pruning
    pruneToolResults,
    pruneThinkingBlocks,
    pruneToolInputs,
    softPrune,
    protectedBoundary,

    -- * Horizon state
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), turnBoundaryIndices)
import Elwood.Config (PruningConfig (..))

-- | Conservative overhead estimate for the pruning indicator text.
indicatorOverhead :: Int
indicatorOverhead = 60

-- | Soft-prune a text value: keep the first @headChars@ and last @tailChars@
-- characters with an indicator in between showing how much was removed.
-- Returns the text unchanged if it's short enough to fit without pruning.
softPrune :: Int -> Int -> Text -> Text
softPrune headC tailC content
  | total <= headC + tailC + indicatorOverhead = content
  | otherwise =
      let headPart = T.take headC content
          tailPart = T.takeEnd tailC content
          pruned = total - headC - tailC
       in headPart
            <> "\n\n... [pruned "
            <> T.pack (show pruned)
            <> " of "
            <> T.pack (show total)
            <> " characters] ...\n\n"
            <> tailPart
  where
    total = T.length content

-- | Find the message index that marks the boundary of protected turns.
-- Returns the index of the Nth-from-last user message containing a
-- 'TextBlock' (i.e. a turn boundary — human-initiated input).
-- Messages at or after this index are protected from pruning.
--
-- If fewer than N turn boundaries exist, returns 0 (protect everything).
protectedBoundary :: Int -> [ClaudeMessage] -> Int
protectedBoundary keepN msgs
  | keepN <= 0 = length msgs -- protect nothing
  | otherwise =
      case drop (length bs - keepN) bs of
        (i : _) -> i
        [] -> 0 -- fewer turns than keepN → protect everything
  where
    bs = turnBoundaryIndices msgs

-- | Replace tool-result content with soft-pruned versions in messages before
-- the effective horizon.  The effective horizon is the minimum of the given
-- horizon and the protected boundary (last N turns are never pruned).
-- Error results ('isError' = True) are never pruned.
--
-- The horizon is clamped to the list length so callers need not bounds-check.
pruneToolResults :: PruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolResults cfg n msgs =
  let boundary = protectedBoundary cfg.keepTurns msgs
      effectiveHorizon = max 0 (min n boundary)
      (prefix, suffix) = splitAt effectiveHorizon msgs
   in map (pruneMessage cfg) prefix ++ suffix

-- | Prune tool results inside a single message.
-- Only user messages can contain 'ToolResultBlock's, but we pattern-match
-- exhaustively for safety.
pruneMessage :: PruningConfig -> ClaudeMessage -> ClaudeMessage
pruneMessage cfg (ClaudeMessage User blocks) =
  ClaudeMessage User (map (pruneBlock cfg) blocks)
pruneMessage _ msg = msg

-- | Soft-prune the content of a non-error 'ToolResultBlock'.
pruneBlock :: PruningConfig -> ContentBlock -> ContentBlock
pruneBlock cfg (ToolResultBlock tid content False) =
  ToolResultBlock tid (softPrune cfg.headChars cfg.tailChars content) False
pruneBlock _ block = block

-- ---------------------------------------------------------------------------
-- Thinking block pruning
-- ---------------------------------------------------------------------------

-- | Strip thinking blocks from assistant messages before the protected
-- boundary.  Messages within the last @keepN@ turns are left untouched.
--
-- NOTE: Anthropic's context editing API beta (context-management-2025-06-27)
-- offers a server-side @clear_thinking_20251015@ strategy that does the same
-- thing.  We prune client-side to avoid the beta dependency for now.
pruneThinkingBlocks :: Maybe Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneThinkingBlocks Nothing msgs = msgs
pruneThinkingBlocks (Just keepN) msgs =
  let boundary = protectedBoundary keepN msgs
      (prefix, suffix) = splitAt boundary msgs
   in map stripThinking prefix ++ suffix

-- | Remove thinking blocks from a single assistant message.
stripThinking :: ClaudeMessage -> ClaudeMessage
stripThinking (ClaudeMessage Assistant blocks) =
  ClaudeMessage Assistant (filter (not . isThinking) blocks)
stripThinking msg = msg

isThinking :: ContentBlock -> Bool
isThinking (ThinkingBlock _ _) = True
isThinking (RedactedThinkingBlock _) = True
isThinking _ = False

-- ---------------------------------------------------------------------------
-- Tool input pruning
-- ---------------------------------------------------------------------------

-- | Soft-prune large tool use inputs in assistant messages before the
-- protected boundary.  Inputs exceeding @threshold@ characters (when
-- serialized to JSON text) are replaced with a soft-pruned text summary.
pruneToolInputs :: PruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolInputs cfg n msgs =
  case cfg.toolInputThreshold of
    Nothing -> msgs
    Just threshold ->
      let boundary = protectedBoundary cfg.keepTurns msgs
          effectiveHorizon = max 0 (min n boundary)
          (prefix, suffix) = splitAt effectiveHorizon msgs
       in map (pruneAssistantInputs cfg threshold) prefix ++ suffix

pruneAssistantInputs :: PruningConfig -> Int -> ClaudeMessage -> ClaudeMessage
pruneAssistantInputs cfg threshold (ClaudeMessage Assistant blocks) =
  ClaudeMessage Assistant (map (pruneToolInput cfg threshold) blocks)
pruneAssistantInputs _ _ msg = msg

pruneToolInput :: PruningConfig -> Int -> ContentBlock -> ContentBlock
pruneToolInput cfg threshold (ToolUseBlock tid name input)
  | let serialized = decodeUtf8 (LBS.toStrict (encode input)),
    T.length serialized > threshold =
      -- Wrap in an object to preserve the JSON type contract (API expects object for input)
      let pruned = softPrune cfg.headChars cfg.tailChars serialized
       in ToolUseBlock tid name (object ["_pruned" .= pruned])
pruneToolInput _ _ block = block

-- ---------------------------------------------------------------------------
-- Horizon state
-- ---------------------------------------------------------------------------

-- | Per-session prune horizon map.  In-memory only; resets on process
-- restart (which is correct because the API cache is also lost).
newtype PruneHorizons = PruneHorizons (TVar (Map Text Int))

-- | Create an empty 'PruneHorizons'.
newPruneHorizons :: IO PruneHorizons
newPruneHorizons = PruneHorizons <$> newTVarIO Map.empty

-- | Atomically read the current horizon for a session and, when the cache
-- has expired, advance it to the current message count.
--
-- Returns the horizon value that should be used for this request.
getAndUpdateHorizon ::
  PruneHorizons ->
  -- | Session ID
  Text ->
  -- | Current message count (length of history)
  Int ->
  -- | Whether the Anthropic cache has expired
  Bool ->
  IO Int
getAndUpdateHorizon (PruneHorizons tvar) sessionId msgCount cacheExpired =
  atomically $ do
    m <- readTVar tvar
    let current = Map.findWithDefault 0 sessionId m
    if cacheExpired
      then do
        writeTVar tvar (Map.insert sessionId msgCount m)
        pure msgCount
      else pure (min current msgCount)
