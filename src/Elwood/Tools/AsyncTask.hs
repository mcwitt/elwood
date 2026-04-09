module Elwood.Tools.AsyncTask
  ( -- * Types
    TaskId (..),
    AsyncTaskStore,

    -- * Store Operations
    newAsyncTaskStore,
    insertTask,
    storeTtlSeconds,
    toMicroseconds,

    -- * Tools
    mkCheckTaskTool,
    mkAwaitTaskTool,
    mkCancelTaskTool,
    cancelAllTasks,
  )
where

import Control.Concurrent.Async (Async, poll, waitCatch)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Elwood.Claude.AgentLoop (AgentResult (..))
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Types
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import System.Timeout (timeout)

-- | Opaque identifier for an async task
newtype TaskId = TaskId {unTaskId :: Text}
  deriving newtype (Show, Eq, Ord)

-- | A running or completed async task
data AsyncTask = AsyncTask
  { label :: Text,
    result :: Async AgentResult,
    createdAt :: UTCTime
  }

-- | Shared store for async delegate tasks
data AsyncTaskStore = AsyncTaskStore
  { tasks :: TVar (Map TaskId AsyncTask),
    ttlSeconds :: NominalDiffTime
  }

-- | Create a new empty async task store
newAsyncTaskStore :: NominalDiffTime -> IO AsyncTaskStore
newAsyncTaskStore ttl = do
  tv <- newTVarIO Map.empty
  pure AsyncTaskStore {tasks = tv, ttlSeconds = ttl}

-- | Insert an async task into the store.
-- The task must be inserted before the associated thread is spawned
-- to avoid a race where check_task returns "Unknown task" for a
-- fast-completing task.
insertTask :: AsyncTaskStore -> TaskId -> Text -> Async AgentResult -> IO ()
insertTask store tid taskLabel a = do
  now <- getCurrentTime
  atomically $ do
    taskMap <- readTVar store.tasks
    writeTVar store.tasks (Map.insert tid (AsyncTask taskLabel a now) taskMap)

-- | Get the store TTL in seconds
storeTtlSeconds :: AsyncTaskStore -> NominalDiffTime
storeTtlSeconds = (.ttlSeconds)

-- | Convert NominalDiffTime to microseconds for System.Timeout.timeout,
-- clamping to maxBound to avoid Int overflow.
toMicroseconds :: NominalDiffTime -> Int
toMicroseconds dt =
  let secs = nominalDiffTimeToSeconds dt
      micros = secs * 1_000_000
   in if micros > fromIntegral (maxBound :: Int)
        then maxBound
        else round micros

-- | Parsed check_task input
data CheckTaskInput = CheckTaskInput
  { taskId :: Maybe TaskId,
    timeoutDuration :: NominalDiffTime
  }

-- | Parse an optional task_id field from a JSON object
parseOptionalTaskId :: KM.KeyMap Value -> Either Text (Maybe TaskId)
parseOptionalTaskId obj = case KM.lookup "task_id" obj of
  Just (Aeson.String t)
    | T.null (T.strip t) -> Left "task_id must not be empty"
    | otherwise -> Right (Just (TaskId t))
  Just _ -> Left "Invalid 'task_id' parameter (must be a string)"
  Nothing -> Right Nothing

-- | Parse a required task_id field from a JSON object
parseRequiredTaskId :: KM.KeyMap Value -> Either Text TaskId
parseRequiredTaskId obj = do
  mTid <- parseOptionalTaskId obj
  case mTid of
    Just tid -> Right tid
    Nothing -> Left "Missing required 'task_id' parameter"

-- | Parse check_task input
parseCheckTaskInput :: Value -> Either Text CheckTaskInput
parseCheckTaskInput (Aeson.Object obj) = do
  tid <- parseOptionalTaskId obj
  dur <- case KM.lookup "timeout_seconds" obj of
    Just (Aeson.Number n) ->
      let i = round n :: Int
       in if i < 0
            then Left "timeout_seconds must be non-negative"
            else Right (fromIntegral i :: NominalDiffTime)
    Just _ -> Left "Invalid 'timeout_seconds' parameter (must be an integer)"
    Nothing -> Right 0
  Right CheckTaskInput {taskId = tid, timeoutDuration = dur}
parseCheckTaskInput _ = Left "Expected object input"

-- | Parsed cancel_task input
newtype CancelTaskInput = CancelTaskInput
  { taskId :: TaskId
  }

-- | Parse cancel_task input
parseCancelTaskInput :: Value -> Either Text CancelTaskInput
parseCancelTaskInput (Aeson.Object obj) = do
  tid <- parseRequiredTaskId obj
  Right CancelTaskInput {taskId = tid}
parseCancelTaskInput _ = Left "Expected object input"

-- | Construct the check_task tool
mkCheckTaskTool :: AsyncTaskStore -> Tool
mkCheckTaskTool store =
  Tool
    { schema =
        ToolSchema
          { name = "check_task",
            description =
              "Check on async delegate tasks. "
                <> "Without task_id, lists all tasks with their ID, label, and status. "
                <> "With task_id, polls that task. Use timeout_seconds to wait for completion "
                <> "(default: 0 = instant poll). Prefer await_task for blocking waits. "
                <> "Finished results are consumed on retrieval.",
            inputSchema = checkTaskSchema
          },
      execute = executeCheckTask store
    }

-- | Parsed await_task input
data AwaitTaskInput = AwaitTaskInput
  { taskId :: TaskId,
    timeoutDuration :: NominalDiffTime
  }

-- | Parse await_task input
parseAwaitTaskInput :: Value -> Either Text AwaitTaskInput
parseAwaitTaskInput (Aeson.Object obj) = do
  tid <- parseRequiredTaskId obj
  dur <- case KM.lookup "timeout_seconds" obj of
    Just (Aeson.Number n) ->
      let i = round n :: Int
       in if i <= 0
            then Left "timeout_seconds must be positive"
            else Right (fromIntegral i :: NominalDiffTime)
    Just _ -> Left "Invalid 'timeout_seconds' parameter (must be an integer)"
    Nothing -> Right 60
  Right AwaitTaskInput {taskId = tid, timeoutDuration = dur}
parseAwaitTaskInput _ = Left "Expected object input"

-- | Construct the await_task tool
mkAwaitTaskTool :: AsyncTaskStore -> Tool
mkAwaitTaskTool store =
  Tool
    { schema =
        ToolSchema
          { name = "await_task",
            description =
              "Block until an async delegate task completes or the timeout is reached. "
                <> "Use this after doing sequential work to collect the result of an async delegate "
                <> "without polling. Returns the task result on success, or a timeout error.",
            inputSchema = awaitTaskSchema
          },
      execute = executeAwaitTask store
    }

-- | JSON Schema for await_task input
awaitTaskSchema :: Value
awaitTaskSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "task_id"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("UUID of the async delegate task to wait for" :: Text)
                ],
            "timeout_seconds"
              .= object
                [ "type" .= ("integer" :: Text),
                  "description" .= ("Maximum seconds to wait (default: 60)" :: Text),
                  "minimum" .= (1 :: Int)
                ]
          ],
      "required" .= (["task_id"] :: [Text])
    ]

-- | Execute await_task: block until the task finishes or timeout elapses
executeAwaitTask :: AsyncTaskStore -> Value -> IO ToolResult
executeAwaitTask store input = case parseAwaitTaskInput input of
  Left err -> pure $ toolError err
  Right ai -> do
    sweepExpired store
    awaitTask store ai.taskId ai.timeoutDuration

-- | Block on a specific task until it completes or the timeout elapses.
-- On completion, consumes the result. On timeout, returns an error.
awaitTask :: AsyncTaskStore -> TaskId -> NominalDiffTime -> IO ToolResult
awaitTask store tid dur = do
  mTask <- atomically $ Map.lookup tid <$> readTVar store.tasks
  case mTask of
    Nothing -> pure $ toolError $ "Unknown task: " <> tid.unTaskId
    Just task -> do
      mResult <- timeout (toMicroseconds dur) (waitCatch task.result)
      case mResult of
        Nothing ->
          pure $
            toolError $
              "Timed out waiting for task " <> tid.unTaskId <> " (" <> task.label <> ") after " <> T.pack (show (round dur :: Int)) <> "s."
        Just r -> consumeAndFormat store tid r

-- | Construct the cancel_task tool
mkCancelTaskTool :: AsyncTaskStore -> Tool
mkCancelTaskTool store =
  Tool
    { schema =
        ToolSchema
          { name = "cancel_task",
            description =
              "Cancel a running async delegate task. "
                <> "The task is cancelled and removed from the store. This is not recoverable.",
            inputSchema = cancelTaskSchema
          },
      execute = executeCancelTask store
    }

-- | JSON Schema for check_task input
checkTaskSchema :: Value
checkTaskSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "task_id"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("UUID of an async delegate task to check" :: Text)
                ],
            "timeout_seconds"
              .= object
                [ "type" .= ("integer" :: Text),
                  "description" .= ("Seconds to wait for completion (default: 0 = instant poll)" :: Text),
                  "minimum" .= (0 :: Int)
                ]
          ]
    ]

-- | JSON Schema for cancel_task input
cancelTaskSchema :: Value
cancelTaskSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "task_id"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("UUID of the async delegate task to cancel" :: Text)
                ]
          ],
      "required" .= (["task_id"] :: [Text])
    ]

-- | Execute check_task
executeCheckTask :: AsyncTaskStore -> Value -> IO ToolResult
executeCheckTask store input = case parseCheckTaskInput input of
  Left err -> pure $ toolError err
  Right ci -> do
    sweepExpired store
    case ci.taskId of
      Nothing -> listAllTasks store
      Just tid -> pollTask store tid ci.timeoutDuration

-- | Execute cancel_task.
-- The task is removed from the store atomically, then cancelled.
-- If cancellation throws (e.g. thread already dead), the task is still
-- removed — this is intentional since a failed cancel means the thread
-- is already gone.
executeCancelTask :: AsyncTaskStore -> Value -> IO ToolResult
executeCancelTask store input = case parseCancelTaskInput input of
  Left err -> pure $ toolError err
  Right si -> do
    mTask <- atomically $ do
      taskMap <- readTVar store.tasks
      case Map.lookup si.taskId taskMap of
        Nothing -> pure Nothing
        Just task -> do
          writeTVar store.tasks (Map.delete si.taskId taskMap)
          pure (Just task)
    case mTask of
      Nothing -> pure $ toolError $ "Unknown task: " <> si.taskId.unTaskId
      Just task -> do
        Async.cancel task.result
          `catch` \(_ :: SomeException) -> pure ()
        pure $ toolSuccess $ "Task " <> si.taskId.unTaskId <> " (" <> task.label <> ") cancelled."

-- | List all tasks with status, sorted by creation time (newest first)
listAllTasks :: AsyncTaskStore -> IO ToolResult
listAllTasks store = do
  taskMap <- atomically $ readTVar store.tasks
  if Map.null taskMap
    then pure $ toolSuccess "No async tasks."
    else do
      entries <- mapM formatEntry (Map.toList taskMap)
      let sorted = sortOn (Down . snd) entries
      pure $ toolSuccess $ T.intercalate "\n" (map fst sorted)
  where
    formatEntry :: (TaskId, AsyncTask) -> IO (Text, UTCTime)
    formatEntry (tid, task) = do
      mResult <- poll task.result
      let status = case mResult of
            Nothing -> "running"
            Just (Right (AgentSuccess _ _)) -> "completed"
            Just (Right AgentCancelled) -> "cancelled"
            Just (Right (AgentError _)) -> "failed"
            Just (Left _) -> "failed"
      pure (tid.unTaskId <> " (" <> task.label <> "): " <> status, task.createdAt)

-- | Poll a specific task, optionally waiting.
-- Finished tasks are consumed: removed from the store after returning their result.
pollTask :: AsyncTaskStore -> TaskId -> NominalDiffTime -> IO ToolResult
pollTask store tid dur = do
  mTask <- atomically $ Map.lookup tid <$> readTVar store.tasks
  case mTask of
    Nothing -> pure $ toolError $ "Unknown task: " <> tid.unTaskId
    Just task
      | dur <= 0 -> do
          -- Instant poll
          mResult <- poll task.result
          case mResult of
            Nothing -> pure $ toolSuccess $ "Task " <> tid.unTaskId <> " (" <> task.label <> ") is still running."
            Just r -> consumeAndFormat store tid r
      | otherwise -> do
          -- Wait with timeout
          mResult <- timeout (toMicroseconds dur) (waitCatch task.result)
          case mResult of
            Nothing -> pure $ toolSuccess $ "Task " <> tid.unTaskId <> " (" <> task.label <> ") is still running after " <> T.pack (show (round dur :: Int)) <> "s wait."
            Just r -> consumeAndFormat store tid r

-- | Remove a finished task from the store and format its result.
consumeAndFormat :: AsyncTaskStore -> TaskId -> Either SomeException AgentResult -> IO ToolResult
consumeAndFormat store tid r = do
  atomically $ do
    taskMap <- readTVar store.tasks
    writeTVar store.tasks (Map.delete tid taskMap)
  formatResult r

-- | Format a completed task result
formatResult :: Either SomeException AgentResult -> IO ToolResult
formatResult (Right (AgentSuccess text _)) = pure $ toolSuccess text
formatResult (Right (AgentError err)) = pure $ toolError err
formatResult (Right AgentCancelled) = pure $ toolError "Task was cancelled"
formatResult (Left exc) = pure $ toolError $ "Task failed with exception: " <> T.pack (show exc)

-- | Cancel all running async tasks. Returns the number of tasks cancelled.
-- Uses 'Async.uninterruptibleCancel' so that an async exception arriving
-- mid-loop cannot leave drained tasks uncancelled.
cancelAllTasks :: AsyncTaskStore -> IO Int
cancelAllTasks store = do
  tasks <- atomically $ do
    taskMap <- readTVar store.tasks
    writeTVar store.tasks Map.empty
    pure (Map.elems taskMap)
  mapM_ (Async.uninterruptibleCancel . (.result)) tasks
  pure (length tasks)

-- | Sweep expired entries from the store.
-- Safe to remove regardless of running status because async tasks are wrapped
-- with a timeout at most equal to the TTL — by the time TTL expires, the task
-- is guaranteed to have completed or timed out.
sweepExpired :: AsyncTaskStore -> IO ()
sweepExpired store = do
  now <- getCurrentTime
  atomically $ sweepExpiredSTM store now

-- | STM sweep of expired entries. Only writes when entries are actually removed.
sweepExpiredSTM :: AsyncTaskStore -> UTCTime -> STM ()
sweepExpiredSTM store now = do
  taskMap <- readTVar store.tasks
  let alive = Map.filter (\t -> diffUTCTime now t.createdAt < store.ttlSeconds) taskMap
  when (Map.size alive /= Map.size taskMap) $
    writeTVar store.tasks alive
