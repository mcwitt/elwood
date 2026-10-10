module Elwood.Approval (ApprovalResult (..), ApprovalCoordinator, Decision (..), newApprovalCoordinator, withApproval, withCancellableApproval, respondToApproval, formatApprovalRequest, parseCallbackData) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.Either (fromLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Elwood.Claude.Types (ToolName (..))
import Elwood.Positive (Positive (getPositive))

data ApprovalResult = Approved | Denied | TimedOut deriving stock (Show, Eq)

data Decision = Approve | Deny deriving stock (Show, Eq)

newtype ApprovalCoordinator = ApprovalCoordinator (TVar (Map.Map UUID (TMVar ApprovalResult, TVar Bool)))

newApprovalCoordinator :: IO ApprovalCoordinator
newApprovalCoordinator = ApprovalCoordinator <$> newTVarIO Map.empty

withApproval :: ApprovalCoordinator -> Positive -> (UUID -> IO ()) -> IO ApprovalResult
withApproval (ApprovalCoordinator pending) seconds notify = do
  key <- UUID.nextRandom
  result <- newEmptyTMVarIO
  timer <- registerDelay (fromInteger $ min (toInteger (maxBound :: Int)) (toInteger seconds.getPositive * 1000000))
  bracket
    (atomically $ modifyTVar' pending (Map.insert key (result, timer)))
    (\() -> atomically $ modifyTVar' pending (Map.delete key))
    ( \() -> do
        notify key
        atomically $ do
          outcome <- readTMVar result `orElse` (readTVar timer >>= check >> pure TimedOut)
          modifyTVar' pending (Map.delete key)
          pure outcome
    )

-- Cancelling an active turn also releases an outstanding approval wait.
withCancellableApproval :: Maybe (TVar Bool) -> ApprovalCoordinator -> Positive -> (UUID -> IO ()) -> IO ApprovalResult
withCancellableApproval Nothing = withApproval
withCancellableApproval (Just token) = \coordinator seconds notify -> do
  cancelled <- readTVarIO token
  if cancelled
    then pure Denied
    else
      fromLeft Denied <$> race (withApproval coordinator seconds notify) (atomically (readTVar token >>= check))

respondToApproval :: ApprovalCoordinator -> UUID -> ApprovalResult -> IO Bool
respondToApproval (ApprovalCoordinator pending) key decision = atomically $ do
  requests <- readTVar pending
  case Map.lookup key requests of
    Nothing -> pure False
    Just (result, timer) -> do
      expired <- readTVar timer
      won <- tryPutTMVar result (if expired then TimedOut else decision)
      writeTVar pending (Map.delete key requests)
      pure (won && not expired)

formatApprovalRequest :: ToolName -> Text -> Text
formatApprovalRequest (ToolName name) input =
  T.unlines ["**Tool Approval Required**", "", "**Tool:** `" <> name <> "`", "**Input:**", "```", input, "```", "Do you want to allow this action?"]

parseCallbackData :: Text -> Maybe (Decision, UUID)
parseCallbackData input = case T.splitOn ":" input of
  ["approve", key] -> (Approve,) <$> UUID.fromText key
  ["deny", key] -> (Deny,) <$> UUID.fromText key
  _ -> Nothing
