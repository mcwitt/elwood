module Elwood.Tools.AsyncTask
  ( TaskId (..),
    AsyncTaskStore,
    newAsyncTaskStore,
    insertTask,
    startTask,
    scopeTasks,
    forTurn,
    isTurnCancelled,
    cancelTurn,
    closeTaskStore,
    toMicroseconds,
    mkCheckTaskTool,
    mkAwaitTaskTool,
    mkCancelTaskTool,
    cancelAllTasks,
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, finally, mask)
import Control.Monad (filterM, void, when)
import Data.Aeson (withObject, (.:!))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Elwood.Claude.AgentLoop (AgentResult (..), formatExhaustion)
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types
import System.Timeout (timeout)

newtype TaskId = TaskId {unTaskId :: Text} deriving stock (Show, Eq, Ord)

data Task = Task {label :: Text, result :: Async AgentResult, completed :: MVar (Maybe UTCTime), owner :: Maybe Text, origin :: Maybe (TVar Bool)}

data AsyncTaskStore = AsyncTaskStore {tasks :: MVar (Bool, Map.Map TaskId Task), ttl :: NominalDiffTime, scope :: Maybe Text, turn :: Maybe (TVar Bool)}

newAsyncTaskStore :: NominalDiffTime -> IO AsyncTaskStore
newAsyncTaskStore ttl = AsyncTaskStore <$> newMVar (False, Map.empty) <*> pure ttl <*> pure Nothing <*> pure Nothing

scopeTasks :: Text -> AsyncTaskStore -> AsyncTaskStore
scopeTasks scope store = store {scope = Just scope}

forTurn :: TVar Bool -> AsyncTaskStore -> AsyncTaskStore
forTurn token store = store {turn = Just token}

isTurnCancelled :: AsyncTaskStore -> IO Bool
isTurnCancelled store = maybe (pure False) readTVarIO store.turn

cancelTurn :: TVar Bool -> AsyncTaskStore -> IO Int
cancelTurn token store = length <$> removeTasks store False (\_ task -> task.origin == Just token)

toMicroseconds :: NominalDiffTime -> Int
toMicroseconds = fromInteger . max 0 . min (toInteger (maxBound :: Int)) . floor . (* 1000000)

-- Registration and child acquisition share one exception boundary.
startTask :: AsyncTaskStore -> TaskId -> Text -> IO AgentResult -> IO ()
startTask store key label action = mask $ \restore -> modifyMVar_ store.tasks $ \(closed, current) -> do
  admit store key closed current
  finished <- newMVar Nothing
  child <- Async.async (restore action `finally` (getCurrentTime >>= \now -> modifyMVar_ finished (const (pure (Just now)))))
  pure (False, Map.insert key (Task label child finished store.scope store.turn) current)

insertTask :: AsyncTaskStore -> TaskId -> Text -> Async AgentResult -> IO ()
insertTask store key label child = modifyMVar_ store.tasks $ \(closed, current) -> do
  admit store key closed current
  done <- Async.poll child
  now <- getCurrentTime
  finished <- newMVar (now <$ done)
  pure (False, Map.insert key (Task label child finished store.scope store.turn) current)

admit :: AsyncTaskStore -> TaskId -> Bool -> Map.Map TaskId Task -> IO ()
admit store key closed current = do
  cancelled <- maybe (pure False) readTVarIO store.turn
  when (closed || cancelled || Map.member key current) (ioError (userError "Task store closed, turn cancelled, or duplicate task ID"))

visible :: AsyncTaskStore -> Task -> Bool
visible store task = maybe True (\owner -> task.owner == Just owner) store.scope

snapshot :: AsyncTaskStore -> IO (Map.Map TaskId Task)
snapshot store = modifyMVar store.tasks $ \(closed, current) -> do
  now <- getCurrentTime
  alive <- filterM (retain now . snd) (Map.toList current)
  let next = Map.fromList alive
  pure ((closed, next), Map.filter (visible store) next)
  where
    retain now task = do
      done <- Async.poll task.result
      case done of
        Nothing -> pure True
        Just _ -> modifyMVar task.completed $ \completed ->
          let timestamp = fromMaybe now completed
           in pure (Just timestamp, diffUTCTime now timestamp < store.ttl)

mkCheckTaskTool :: AsyncTaskStore -> Tool
mkCheckTaskTool store = B.tool "check_task" "Observe tasks; completed results remain readable until expiry or cancellation." fields parser $ \(key, seconds) -> do
  current <- snapshot store
  case key of
    Just tid -> observe current tid seconds False
    Nothing -> do
      entries <-
        mapM
          ( \(k, t) -> do
              result <- Async.poll t.result
              let status = case result of
                    Nothing -> "running"
                    Just (Right (AgentSuccess {})) -> "completed"
                    Just (Right (AgentExhausted {})) -> "exhausted"
                    Just (Right AgentCancelled) -> "cancelled"
                    _ -> "failed"
              pure (k.unTaskId <> " (" <> t.label <> "): " <> status)
          )
          (Map.toList current)
      pure $ toolSuccess $ if null entries then "No async tasks." else T.intercalate "\n" entries
  where
    fields = B.schema [("task_id", B.field "string" "Task ID; omit to list tasks"), ("timeout_seconds", B.field "integer" "Seconds to wait, default 0")] []
    parser = withObject "check_task" $ \value ->
      (,)
        <$> ( do
                key <- value .:! "task_id"
                traverse (\t -> if T.null (T.strip t) then fail "task_id must not be empty" else pure (TaskId t)) key
            )
        <*> B.optionalInteger value "timeout_seconds" 0 0 (maxBound `div` 1000000)

mkAwaitTaskTool :: AsyncTaskStore -> Tool
mkAwaitTaskTool store = B.tool "await_task" "Wait for a task; observing does not consume its result." fields parser $ \(key, seconds) -> snapshot store >>= \current -> observe current key seconds True
  where
    fields = B.schema [("task_id", B.field "string" "Task ID"), ("timeout_seconds", B.field "integer" "Seconds to wait, default 60")] ["task_id"]
    parser = withObject "await_task" $ \value -> ((,) . TaskId <$> B.nonempty value "task_id") <*> B.optionalInteger value "timeout_seconds" 60 1 (maxBound `div` 1000000)

observe :: Map.Map TaskId Task -> TaskId -> Int -> Bool -> IO ToolResult
observe current key seconds awaiting = case Map.lookup key current of
  Nothing -> pure $ toolError ("Unknown task: " <> key.unTaskId)
  Just task -> do
    result <- if seconds == 0 then Async.poll task.result else timeout (seconds * 1000000) (Async.waitCatch task.result)
    pure $ maybe (if awaiting then toolError ("Timed out waiting for task " <> key.unTaskId) else toolSuccess ("Task " <> key.unTaskId <> " is still running.")) formatResult result

formatResult :: Either SomeException AgentResult -> ToolResult
formatResult = \case
  Right (AgentSuccess text _) -> toolSuccess text
  Right (AgentTruncated text _) -> toolError ("[truncated] " <> text)
  Right (AgentExhausted info) -> toolError (formatExhaustion info)
  Right (AgentError err) -> toolError err
  Right AgentCancelled -> toolError (taggedError Cancelled "Task was cancelled")
  Left exception -> toolError (taggedError Unexpected (T.pack (show exception)))

mkCancelTaskTool :: AsyncTaskStore -> Tool
mkCancelTaskTool store = B.tool "cancel_task" "Cancel and remove a task." (B.schema [("task_id", B.field "string" "Task ID")] ["task_id"]) (withObject "cancel_task" (fmap TaskId . (`B.nonempty` "task_id"))) $ \key -> do
  found <- removeTasks store False (\tid task -> tid == key && visible store task)
  pure $ if null found then toolError ("Unknown task: " <> key.unTaskId) else toolSuccess ("Task " <> key.unTaskId <> " cancelled.")

removeTasks :: AsyncTaskStore -> Bool -> (TaskId -> Task -> Bool) -> IO [Task]
removeTasks store close select = mask $ \_ -> do
  removed <- modifyMVar store.tasks $ \(closed, current) ->
    let (removed, remaining) = Map.partitionWithKey select current
     in pure ((closed || close, remaining), Map.elems removed)
  mapM_ (Async.uninterruptibleCancel . (.result)) removed
  pure removed

cancelAllTasks :: AsyncTaskStore -> IO Int
cancelAllTasks store = length <$> removeTasks store False (\_ -> visible store)

closeTaskStore :: AsyncTaskStore -> IO ()
closeTaskStore store = void (removeTasks store True (\_ _ -> True))
