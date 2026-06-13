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

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    registerDelay,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, forever, unless, void, when)
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
    path :: FilePath
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
  pure CallbackStore {callbacks = cbs, version = ver, path = p}

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

-- | Write the whole store to disk atomically (temp file + rename).
persist :: CallbackStore -> IO ()
persist store = do
  m <- readTVarIO store.callbacks
  let tmp = store.path <> ".tmp"
  encodeFile tmp (Map.elems m)
  renameFile tmp store.path

bumpVersion :: CallbackStore -> STM ()
bumpVersion store = modifyTVar' store.version (+ 1)

-- | Insert a callback and persist. Returns 'Left' if the pending count would
-- exceed 'maxPendingCallbacks'.
scheduleCallback :: CallbackStore -> Callback -> IO (Either Text ())
scheduleCallback store cb = do
  ok <- atomically $ do
    m <- readTVar store.callbacks
    if Map.size m >= maxPendingCallbacks
      then pure False
      else do
        writeTVar store.callbacks (Map.insert cb.id_ cb m)
        bumpVersion store
        pure True
  if ok
    then persist store >> pure (Right ())
    else pure (Left ("Too many pending callbacks (max " <> T.pack (show maxPendingCallbacks) <> ")"))

-- | All pending callbacks, earliest fire time first.
listCallbacks :: CallbackStore -> IO [Callback]
listCallbacks store = sortOn (.fireAt) . Map.elems <$> readTVarIO store.callbacks

-- | Remove a callback by id and persist. 'False' if not present.
cancelCallback :: CallbackStore -> CallbackId -> IO Bool
cancelCallback store cid = do
  found <- atomically $ do
    m <- readTVar store.callbacks
    if Map.member cid m
      then do
        writeTVar store.callbacks (Map.delete cid m)
        bumpVersion store
        pure True
      else pure False
  when found (persist store)
  pure found

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

-- | Remove all currently-due callbacks, persist the removal, then run the fire
-- action for each. Persisting before firing gives at-most-once semantics: a
-- crash between the persist and the fire loses that callback rather than
-- re-firing it on restart. The fire action is run synchronously here; the
-- production caller ('runScheduler') wraps it to fork and guard exceptions.
fireDue :: CallbackStore -> (Callback -> IO ()) -> IO ()
fireDue store fire = do
  now <- getCurrentTime
  due <- atomically $ do
    m <- readTVar store.callbacks
    let (dueList, rest) = dueCallbacks now m
    unless (null dueList) $ do
      writeTVar store.callbacks rest
      bumpVersion store
    pure dueList
  unless (null due) (persist store)
  forM_ due fire

-- | Run the scheduler loop forever: fire everything due, then wait until the
-- next fire time or until the store changes. Each fire is forked and guarded so
-- a slow or failing turn cannot stall the loop. @fire@ injects the woken turn.
runScheduler :: Logger -> CallbackStore -> (Callback -> IO ()) -> IO ()
runScheduler lgr store fire = forever $ do
  fireDue store firedSafe
  waitForNext store
  where
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
