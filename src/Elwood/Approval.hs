{-# LANGUAGE StrictData #-}

module Elwood.Approval
  ( -- * Types
    ApprovalResult (..),
    ApprovalCoordinator (..),

    -- * Coordinator Operations
    newApprovalCoordinator,
    requestApproval,
    respondToApproval,

    -- * Helpers
    formatApprovalRequest,
    parseCallbackData,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID

-- | Result of an approval request
data ApprovalResult
  = -- | User approved the action
    Approved
  | -- | User denied the action
    Denied
  | -- | Request timed out waiting for response
    TimedOut
  deriving stock (Show, Eq)

-- | Coordinator for managing pending approval requests
data ApprovalCoordinator = ApprovalCoordinator
  { -- | Map of pending requests awaiting response
    acPendingRequests :: TVar (Map UUID (TMVar ApprovalResult)),
    -- | Timeout in seconds for approval requests
    acTimeoutSeconds :: Int
  }

-- | Create a new approval coordinator
newApprovalCoordinator :: Int -> IO ApprovalCoordinator
newApprovalCoordinator timeoutSeconds = do
  pendingVar <- newTVarIO Map.empty
  pure
    ApprovalCoordinator
      { acPendingRequests = pendingVar,
        acTimeoutSeconds = timeoutSeconds
      }

-- | Request approval and block until response or timeout
--
-- Returns the UUID for the request (to be included in callback data)
-- and blocks until a response is received or timeout occurs.
requestApproval :: ApprovalCoordinator -> IO (UUID, IO ApprovalResult)
requestApproval coordinator = do
  -- Generate unique ID for this request
  requestId <- UUID.nextRandom

  -- Create TMVar for the response
  responseVar <- newEmptyTMVarIO

  -- Register the pending request
  atomically $ modifyTVar' (acPendingRequests coordinator) (Map.insert requestId responseVar)

  -- Start timeout thread
  void $
    forkIO $ do
      threadDelay (acTimeoutSeconds coordinator * 1000000)
      -- Try to mark as timed out (only succeeds if not already responded)
      atomically $ do
        pending <- readTVar (acPendingRequests coordinator)
        case Map.lookup requestId pending of
          Just var -> do
            success <- tryPutTMVar var TimedOut
            when success $ modifyTVar' (acPendingRequests coordinator) (Map.delete requestId)
          Nothing -> pure ()

  -- Return the request ID and an action to wait for the result
  let waitForResult = atomically $ readTMVar responseVar
  pure (requestId, waitForResult)

-- | Respond to a pending approval request
--
-- Returns True if the request was found and responded to,
-- False if the request was not found (already responded or timed out)
respondToApproval :: ApprovalCoordinator -> UUID -> ApprovalResult -> IO Bool
respondToApproval coordinator requestId result = atomically $ do
  pending <- readTVar (acPendingRequests coordinator)
  case Map.lookup requestId pending of
    Just var -> do
      success <- tryPutTMVar var result
      when success $ modifyTVar' (acPendingRequests coordinator) (Map.delete requestId)
      pure success
    Nothing -> pure False

-- | Format a tool use request for display in Telegram
formatApprovalRequest :: Text -> Text -> Text
formatApprovalRequest toolName inputSummary =
  T.unlines
    [ "🔐 *Tool Approval Required*",
      "",
      "*Tool:* `" <> toolName <> "`",
      "*Input:*",
      "```",
      escapeCodeBlock inputSummary,
      "```",
      "Do you want to allow this action?"
    ]
  where
    -- Escape triple backticks inside code blocks
    escapeCodeBlock = T.replace "```" "\\`\\`\\`"

-- | Parse callback data to extract approval decision and request ID
--
-- Callback data format: "approve:<uuid>" or "deny:<uuid>"
parseCallbackData :: Text -> Maybe (Bool, UUID)
parseCallbackData callbackData =
  case T.splitOn ":" callbackData of
    ["approve", uuidText] -> (True,) <$> UUID.fromText uuidText
    ["deny", uuidText] -> (False,) <$> UUID.fromText uuidText
    _ -> Nothing
