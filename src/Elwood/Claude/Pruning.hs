{-# LANGUAGE StrictData #-}

module Elwood.Claude.Pruning
  ( -- * Pruning
    pruneToolResults,
    prunedPlaceholder,

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
import Data.Time (NominalDiffTime)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..))

-- | Placeholder text that replaces pruned tool results.
prunedPlaceholder :: Text
prunedPlaceholder = "[previous tool result]"

-- | Replace tool-result content with a placeholder in messages before the
-- given horizon index.  Messages at index @n@ and beyond are unchanged.
-- Error results ('isError' = True) are never pruned.
--
-- The horizon is clamped to the list length so callers need not bounds-check.
pruneToolResults :: Int -> [ClaudeMessage] -> [ClaudeMessage]
pruneToolResults n msgs =
  let horizon = max 0 (min n (length msgs))
      (prefix, suffix) = splitAt horizon msgs
   in map pruneMessage prefix ++ suffix

-- | Prune tool results inside a single message.
-- Only user messages can contain 'ToolResultBlock's, but we pattern-match
-- exhaustively for safety.
pruneMessage :: ClaudeMessage -> ClaudeMessage
pruneMessage (ClaudeMessage User blocks) =
  ClaudeMessage User (map pruneBlock blocks)
pruneMessage msg = msg

-- | Replace the content of a non-error 'ToolResultBlock' with the placeholder.
pruneBlock :: ContentBlock -> ContentBlock
pruneBlock (ToolResultBlock tid _ False) =
  ToolResultBlock tid prunedPlaceholder False
pruneBlock block = block

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
      else pure current
