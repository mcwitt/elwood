{-# LANGUAGE StrictData #-}

module Elwood.Approval
  ( -- * Types
    ApprovalResult (..),
    ApprovalCoordinator (..),
    Decision (..),

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
    pendingRequests :: TVar (Map UUID (TMVar ApprovalResult)),
    -- | Timeout in seconds for approval requests
    timeoutSeconds :: Int
  }

-- | Create a new approval coordinator
newApprovalCoordinator :: Int -> IO ApprovalCoordinator
newApprovalCoordinator ts = do
  pendingVar <- newTVarIO Map.empty
  pure
    ApprovalCoordinator
      { pendingRequests = pendingVar,
        timeoutSeconds = ts
      }

-- | Request approval and block until response or timeout
--
-- Returns the UUID for the request (to be included in callback data)
-- and blocks until a response is received or timeout occurs.
requestApproval :: ApprovalCoordinator -> IO (UUID, IO ApprovalResult)
requestApproval coordinator = do
  -- Generate unique ID for this request
  requestId_ <- UUID.nextRandom

  -- Create TMVar for the response
  responseVar <- newEmptyTMVarIO

  -- Register the pending request
  atomically $ modifyTVar' coordinator.pendingRequests (Map.insert requestId_ responseVar)

  -- Start timeout thread
  void $
    forkIO $ do
      threadDelay (coordinator.timeoutSeconds * 1000000)
      -- Try to mark as timed out (only succeeds if not already responded)
      atomically $ do
        pending <- readTVar coordinator.pendingRequests
        case Map.lookup requestId_ pending of
          Just var -> do
            success <- tryPutTMVar var TimedOut
            when success $ modifyTVar' coordinator.pendingRequests (Map.delete requestId_)
          Nothing -> pure ()

  -- Return the request ID and an action to wait for the result
  let waitForResult = atomically $ readTMVar responseVar
  pure (requestId_, waitForResult)

-- | Respond to a pending approval request
--
-- Returns True if the request was found and responded to,
-- False if the request was not found (already responded or timed out)
respondToApproval :: ApprovalCoordinator -> UUID -> ApprovalResult -> IO Bool
respondToApproval coordinator requestId_ result = atomically $ do
  pending <- readTVar coordinator.pendingRequests
  case Map.lookup requestId_ pending of
    Just var -> do
      success <- tryPutTMVar var result
      when success $ modifyTVar' coordinator.pendingRequests (Map.delete requestId_)
      pure success
    Nothing -> pure False

-- | Format a tool use request for display in Telegram
--
-- Emits standard markdown; the downstream @sendMessageWithKeyboard@ handles
-- conversion to Telegram HTML.
formatApprovalRequest :: Text -> Text -> Text
formatApprovalRequest toolName_ inputSummary =
  T.unlines
    [ "**Tool Approval Required**",
      "",
      "**Tool:** `" <> toolName_ <> "`",
      "**Input:**",
      "```",
      inputSummary,
      "```",
      "Do you want to allow this action?"
    ]

-- | User's decision on a tool approval request
data Decision
  = Approve
  | Deny
  deriving stock (Show, Eq)

-- | Parse callback data to extract approval decision and request ID
--
-- Callback data format: "approve:<uuid>" or "deny:<uuid>"
parseCallbackData :: Text -> Maybe (Decision, UUID)
parseCallbackData callbackData =
  case T.splitOn ":" callbackData of
    ["approve", uuidText] -> (Approve,) <$> UUID.fromText uuidText
    ["deny", uuidText] -> (Deny,) <$> UUID.fromText uuidText
    _ -> Nothing
