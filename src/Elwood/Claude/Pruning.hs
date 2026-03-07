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
import Elwood.Config (PruningConfig (..), ThinkingPruningConfig (..), ToolDirectionConfig (..), ToolPruningConfig (..))

-- | Conservative overhead estimate for the pruning indicator text.
indicatorOverhead :: Int
indicatorOverhead = 60

-- | Soft-prune a text value: keep the first @headChars@ and last @tailChars@
-- characters with an indicator in between showing how much was removed.
-- Returns 'Nothing' if the text is short enough to fit without pruning,
-- or @'Just' pruned@ when content was truncated.
softPrune :: Int -> Int -> Text -> Maybe Text
softPrune headC tailC content
  | total <= headC + tailC + indicatorOverhead = Nothing
  | otherwise =
      let headPart = T.take headC content
          tailPart = T.takeEnd tailC content
          pruned = total - headC - tailC
       in Just $
            headPart
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

-- | Apply a transformation to messages before the effective horizon.
-- The effective horizon is the minimum of the given horizon and the
-- protected boundary (last @keepN@ turns are never modified).
--
-- The horizon is clamped to the list length so callers need not bounds-check.
pruneBeforeHorizon ::
  -- | Number of recent turns to protect
  Int ->
  -- | Cache-aware prune horizon
  Int ->
  -- | Transformation to apply to each message in the prefix
  (ClaudeMessage -> ClaudeMessage) ->
  [ClaudeMessage] ->
  [ClaudeMessage]
pruneBeforeHorizon keepN n f msgs =
  let boundary = protectedBoundary keepN msgs
      effectiveHorizon = max 0 (min n boundary)
      (prefix, suffix) = splitAt effectiveHorizon msgs
   in map f prefix ++ suffix

-- | Replace tool-result content with soft-pruned versions in messages before
-- the effective horizon.
-- Error results ('isError' = True) are never pruned.
pruneToolResults :: PruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolResults cfg n = pruneBeforeHorizon cfg.tools.output.keepTurns n (pruneMessage cfg.tools.output)

-- | Prune tool results inside a single message.
-- Only user messages can contain 'ToolResultBlock's, but we pattern-match
-- exhaustively for safety.
pruneMessage :: ToolDirectionConfig -> ClaudeMessage -> ClaudeMessage
pruneMessage dir (ClaudeMessage User blocks) =
  ClaudeMessage User (map (pruneBlock dir) blocks)
pruneMessage _ msg = msg

-- | Soft-prune the content of a non-error 'ToolResultBlock'.
pruneBlock :: ToolDirectionConfig -> ContentBlock -> ContentBlock
pruneBlock dir (ToolResultBlock tid content False) =
  case softPrune dir.headChars dir.tailChars content of
    Nothing -> ToolResultBlock tid content False
    Just pruned -> ToolResultBlock tid pruned False
pruneBlock _ block = block

-- ---------------------------------------------------------------------------
-- Thinking block pruning
-- ---------------------------------------------------------------------------

-- | Strip thinking blocks from assistant messages before the effective
-- horizon.  The effective horizon is the minimum of the given horizon and
-- the protected boundary (last @keepN@ turns are never pruned).
--
-- This is gated by the cache-aware prune horizon so that thinking blocks
-- are only stripped from messages the API cache has already absorbed,
-- avoiding unnecessary cache invalidation.
--
-- NOTE: Anthropic's context editing API beta (context-management-2025-06-27)
-- offers a server-side @clear_thinking_20251015@ strategy that does the same
-- thing.  We prune client-side to avoid the beta dependency for now.
pruneThinkingBlocks :: Maybe ThinkingPruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneThinkingBlocks Nothing _ = id
pruneThinkingBlocks (Just tcfg) n = pruneBeforeHorizon tcfg.keepTurns n stripThinking

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
-- protected boundary.  Inputs are serialized to JSON text and soft-pruned;
-- only replaced when 'softPrune' returns 'Just' (i.e. the content is large
-- enough to warrant truncation).
pruneToolInputs :: PruningConfig -> Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolInputs cfg n = pruneBeforeHorizon cfg.tools.input.keepTurns n (pruneAssistantInputs cfg.tools.input)

pruneAssistantInputs :: ToolDirectionConfig -> ClaudeMessage -> ClaudeMessage
pruneAssistantInputs dir (ClaudeMessage Assistant blocks) =
  ClaudeMessage Assistant (map (pruneToolInput dir) blocks)
pruneAssistantInputs _ msg = msg

pruneToolInput :: ToolDirectionConfig -> ContentBlock -> ContentBlock
pruneToolInput dir (ToolUseBlock tid name input) =
  let serialized = decodeUtf8 (LBS.toStrict (encode input))
   in case softPrune dir.headChars dir.tailChars serialized of
        Nothing -> ToolUseBlock tid name input
        -- Wrap in an object to preserve the JSON type contract (API expects object for input)
        Just pruned -> ToolUseBlock tid name (object ["_pruned" .= pruned])
pruneToolInput _ block = block

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
