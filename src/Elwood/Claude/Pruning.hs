{-# LANGUAGE StrictData #-}

module Elwood.Claude.Pruning
  ( -- * Pruning
    pruneToolResults,
    softPrune,
    protectedBoundary,

    -- * Horizon state
    PruneHorizons,
    newPruneHorizons,
    getAndUpdateHorizon,

    -- * Constants
    anthropicCacheTtl,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..))
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
      let -- Collect indices of user messages containing a TextBlock (turn boundaries)
          turnIndices =
            [ i
            | (i, ClaudeMessage User blocks) <- zip [0 ..] msgs,
              any isTextBlock blocks
            ]
          isTextBlock (TextBlock _) = True
          isTextBlock _ = False
       in case drop (length turnIndices - keepN) turnIndices of
            (i : _) -> i
            [] -> 0 -- fewer turns than keepN → protect everything

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
-- Horizon state
-- ---------------------------------------------------------------------------

-- | Anthropic ephemeral prompt-cache TTL (5 minutes).
-- Must stay in sync with the @cache_control@ object sent in API requests.
anthropicCacheTtl :: NominalDiffTime
anthropicCacheTtl = 300

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
