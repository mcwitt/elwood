{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Elwood.Scheduler
  ( -- * Types
    CallbackId (..),
    Callback (..),
    CallbackStore,

    -- * Store
    newCallbackStore,
    scheduleCallback,
    listCallbacks,
    cancelCallback,
    maxPendingCallbacks,

    -- * Timer loop
    runScheduler,

    -- * Firing (exported for testing)
    fireDue,

    -- * Pure helpers (exported for testing)
    dueCallbacks,
    nextWake,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    registerDelay,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, catch, onException, try)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    eitherDecodeFileStrict,
    encodeFile,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Elwood.Event.Types (DeliveryTarget, SessionConfig)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))

-- | Opaque identifier for a scheduled callback (a UUID as text).
newtype CallbackId = CallbackId {unCallbackId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | A pending one-shot callback. @session@ and @deliveryTarget@ are captured
-- from the scheduling turn so the woken turn resumes the same conversation and
-- is delivered to the same chat.
data Callback = Callback
  { id_ :: CallbackId,
    fireAt :: UTCTime,
    prompt :: Text,
    session :: SessionConfig,
    deliveryTarget :: DeliveryTarget,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance ToJSON Callback where
  toJSON c =
    object
      [ "id" .= c.id_,
        "fire_at" .= c.fireAt,
        "prompt" .= c.prompt,
        "session" .= c.session,
        "delivery_target" .= c.deliveryTarget,
        "created_at" .= c.createdAt
      ]

instance FromJSON Callback where
  parseJSON = withObject "Callback" $ \v ->
    Callback
      <$> v .: "id"
      <*> v .: "fire_at"
      <*> v .: "prompt"
      <*> v .: "session"
      <*> v .: "delivery_target"
      <*> v .: "created_at"

-- | Persisted store of pending callbacks. The version 'TVar' is bumped on every
-- mutation so the timer loop can wake early when work is added or removed.
data CallbackStore = CallbackStore
  { callbacks :: TVar (Map CallbackId Callback),
    version :: TVar Int,
    path :: FilePath,
    -- | Serializes disk writes so concurrent mutations (tool threads + the
    -- timer loop) cannot interleave temp-file writes or race the rename.
    writeLock :: MVar ()
  }

-- | Hard sanity cap on pending callbacks, to bound a runaway scheduling loop.
maxPendingCallbacks :: Int
maxPendingCallbacks = 1000

-- | Create a store, loading any pending callbacks from
-- @\<stateDir\>/callbacks.json@. A missing or unreadable file yields an empty
-- store (logged).
newCallbackStore :: Logger -> FilePath -> IO CallbackStore
newCallbackStore lgr stateDir = do
  let p = stateDir </> "callbacks.json"
  loaded <- loadCallbacks lgr p
  cbs <- newTVarIO (Map.fromList [(c.id_, c) | c <- loaded])
  ver <- newTVarIO 0
  lock <- newMVar ()
  pure CallbackStore {callbacks = cbs, version = ver, path = p, writeLock = lock}

loadCallbacks :: Logger -> FilePath -> IO [Callback]
loadCallbacks lgr p = do
  exists <- doesFileExist p
  if not exists
    then pure []
    else do
      r <- eitherDecodeFileStrict p
      case r of
        Right cs -> pure cs
        Left err -> do
          logWarn lgr "Failed to load callbacks; starting empty" [("error", T.pack err)]
          pure []

-- | Write a callback map to disk atomically (temp file + rename). Always
-- called while holding 'writeLock', so the fixed temp path has a single writer.
writeMap :: CallbackStore -> Map CallbackId Callback -> IO ()
writeMap store m = do
  let tmp = store.path <> ".tmp"
  encodeFile tmp (Map.elems m)
  renameFile tmp store.path

-- | Apply a pure transformation to the callback map and durably persist the
-- result, returning the transformation's value. All mutations go through here.
--
-- Correctness properties:
--
--   * 'writeLock' serializes the whole read-modify-persist sequence, so
--     concurrent callers cannot interleave on the shared temp file or the
--     rename, and the map written to disk is exactly the one just committed
--     (no stale snapshot from a separate read).
--   * If the disk write fails, the in-memory map is rolled back to its prior
--     value and the exception is rethrown, so memory never diverges from disk.
--   * The write (and version bump) are skipped when the map is unchanged, so a
--     no-op (e.g. a poll that finds nothing due) does not rewrite the file.
modifyAndPersist :: CallbackStore -> (Map CallbackId Callback -> (Map CallbackId Callback, a)) -> IO a
modifyAndPersist store f =
  withMVar store.writeLock $ \_ -> do
    prior <- readTVarIO store.callbacks
    let (next, a) = f prior
    when (next /= prior) $ do
      atomically $ do
        writeTVar store.callbacks next
        modifyTVar' store.version (+ 1)
      writeMap store next
        `onException` atomically (writeTVar store.callbacks prior)
    pure a

-- | Insert a callback and persist. Returns 'Left' if the pending count would
-- exceed 'maxPendingCallbacks' or the disk write fails (in which case nothing
-- is left in memory).
scheduleCallback :: CallbackStore -> Callback -> IO (Either Text ())
scheduleCallback store cb =
  try (modifyAndPersist store insert) >>= \case
    Left (e :: SomeException) -> pure (Left (T.pack (show e)))
    Right result -> pure result
  where
    insert m
      | Map.size m >= maxPendingCallbacks =
          (m, Left ("Too many pending callbacks (max " <> T.pack (show maxPendingCallbacks) <> ")"))
      | otherwise = (Map.insert cb.id_ cb m, Right ())

-- | All pending callbacks, earliest fire time first.
listCallbacks :: CallbackStore -> IO [Callback]
listCallbacks store = sortOn (.fireAt) . Map.elems <$> readTVarIO store.callbacks

-- | Remove a callback by id and persist. 'Right False' if not present;
-- 'Left' if the disk write fails (the callback is left in place).
cancelCallback :: CallbackStore -> CallbackId -> IO (Either Text Bool)
cancelCallback store cid =
  try (modifyAndPersist store (\m -> (Map.delete cid m, Map.member cid m))) >>= \case
    Left (e :: SomeException) -> pure (Left (T.pack (show e)))
    Right found -> pure (Right found)

-- | Split a map into (due now, not yet due). Pure.
dueCallbacks :: UTCTime -> Map CallbackId Callback -> ([Callback], Map CallbackId Callback)
dueCallbacks now m =
  let (dueMap, restMap) = Map.partition (\c -> c.fireAt <= now) m
   in (Map.elems dueMap, restMap)

-- | The earliest pending fire time, if any. Pure.
nextWake :: Map CallbackId Callback -> Maybe UTCTime
nextWake m
  | Map.null m = Nothing
  | otherwise = Just (minimum [c.fireAt | c <- Map.elems m])

-- | Remove all currently-due callbacks, durably persist the removal, then run
-- the fire action for each. Persisting before firing gives at-most-once
-- semantics: a crash between the persist and the fire loses that callback
-- rather than re-firing it on restart. If the persist fails, 'modifyAndPersist'
-- rolls the removal back (the callbacks stay pending for a later retry) and
-- rethrows, so no work is silently dropped. The fire action is run
-- synchronously here; the production caller ('runScheduler') wraps it to fork
-- and guard exceptions.
fireDue :: CallbackStore -> (Callback -> IO ()) -> IO ()
fireDue store fire = do
  now <- getCurrentTime
  due <- modifyAndPersist store $ \m ->
    let (dueList, rest) = dueCallbacks now m
     in (rest, dueList)
  forM_ due fire

-- | Backoff after a scheduler iteration fails (e.g. a disk write error) so the
-- loop does not spin retrying a persistent failure.
schedulerBackoffMicros :: Int
schedulerBackoffMicros = 5_000_000

-- | Run the scheduler loop forever: fire everything due, then wait until the
-- next fire time or until the store changes. Each fire is forked and guarded so
-- a slow or failing turn cannot stall the loop. The whole iteration is guarded
-- so a persist/IO failure logs and backs off instead of killing the thread
-- (which would silently stop all future callbacks). @fire@ injects the woken
-- turn.
runScheduler :: Logger -> CallbackStore -> (Callback -> IO ()) -> IO ()
runScheduler lgr store fire = forever (iteration `catch` backoff)
  where
    iteration = do
      fireDue store firedSafe
      waitForNext store
    backoff (e :: SomeException) = do
      logError lgr "Scheduler iteration failed; backing off" [("error", T.pack (show e))]
      threadDelay schedulerBackoffMicros
    firedSafe cb = do
      logInfo lgr "Firing scheduled callback" [("id", cb.id_.unCallbackId), ("fire_at", T.pack (show cb.fireAt))]
      void $
        forkIO $
          fire cb
            `catch` \(e :: SomeException) ->
              logError lgr "Callback fire failed" [("id", cb.id_.unCallbackId), ("error", T.pack (show e))]

-- | Block until the soonest pending fire time, or until the store changes.
-- Empty store: block until the version changes. The delay is capped at one hour
-- so far-future callbacks (and any clock changes) are re-evaluated periodically.
waitForNext :: CallbackStore -> IO ()
waitForNext store = do
  ver <- readTVarIO store.version
  m <- readTVarIO store.callbacks
  now <- getCurrentTime
  case nextWake m of
    Nothing -> atomically $ do
      v <- readTVar store.version
      when (v == ver) retry
    Just t -> do
      delayVar <- registerDelay (delayMicros now t)
      atomically $ do
        timedOut <- readTVar delayVar
        v <- readTVar store.version
        when (not timedOut && v == ver) retry

-- | Microseconds to wait until @t@, clamped to [0, 1 hour] to avoid Int
-- overflow on far-future times and to bound re-evaluation latency.
delayMicros :: UTCTime -> UTCTime -> Int
delayMicros now t =
  let secs = realToFrac (diffUTCTime t now) :: Double
      capped = max 0 (min 3600 secs)
   in round (capped * 1e6)
