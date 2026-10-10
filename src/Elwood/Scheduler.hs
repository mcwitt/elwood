module Elwood.Scheduler (CallbackId (..), Callback (..), CallbackStore, newCallbackStore, scheduleCallback, listCallbacks, cancelCallback, maxPendingCallbacks, runScheduler, fireDue, dueCallbacks, nextWake) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, mask_, throwIO)
import Control.Monad (forever, when)
import Data.Aeson
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Elwood.Aeson (recordOptions)
import Elwood.Event.Types (DeliveryTarget, SessionConfig (..))
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logError)
import Elwood.Persistence qualified as Persistence
import Elwood.Workers qualified as Workers
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

newtype CallbackId = CallbackId {unCallbackId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

data Callback = Callback
  { id_ :: CallbackId,
    fireAt :: UTCTime,
    prompt :: Text,
    session :: SessionConfig,
    deliveryTarget :: DeliveryTarget,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Callback where
  toJSON = genericToJSON recordOptions

instance FromJSON Callback where
  parseJSON = genericParseJSON recordOptions {rejectUnknownFields = False}

data CallbackStore = CallbackStore {callbacks :: MVar (Map CallbackId Callback), version :: TVar Integer, path :: FilePath}

maxPendingCallbacks :: Int
maxPendingCallbacks = 1000

newCallbackStore :: Logger -> FilePath -> IO CallbackStore
newCallbackStore _ directory = do
  createDirectoryIfMissing True directory
  let path = directory </> "callbacks.json"
  loaded <- Persistence.readJSON path []
  CallbackStore <$> newMVar (Map.fromList [(value.id_, value) | value <- loaded]) <*> newTVarIO 0 <*> pure path

modifyStore :: CallbackStore -> (Map CallbackId Callback -> (Map CallbackId Callback, a)) -> IO a
modifyStore store change = mask_ $ modifyMVar store.callbacks $ \old -> do
  let (next, result) = change old
  when (old /= next) $ do
    Persistence.writeJSON store.path (Map.elems next)
    atomically (modifyTVar' store.version (+ 1))
  pure (next, result)

failure :: IO (Either Text a) -> IO (Either Text a)
failure action = action `catchSync` \e -> pure (Left (T.pack (show e)))

scheduleCallback :: CallbackStore -> Callback -> IO (Either Text ())
scheduleCallback store value = failure $ modifyStore store $ \current ->
  if Map.size current >= maxPendingCallbacks || Map.member value.id_ current
    then (current, Left "Too many pending callbacks or duplicate callback ID")
    else (Map.insert value.id_ value current, Right ())

listCallbacks :: CallbackStore -> IO [Callback]
listCallbacks store = sortOn (.fireAt) . Map.elems <$> readMVar store.callbacks

cancelCallback :: CallbackStore -> CallbackId -> IO (Either Text Bool)
cancelCallback store key = failure $ modifyStore store $ \current -> (Map.delete key current, Right (Map.member key current))

dueCallbacks :: UTCTime -> Map CallbackId Callback -> ([Callback], Map CallbackId Callback)
dueCallbacks now current = let (due, remaining) = Map.partition ((<= now) . (.fireAt)) current in (sortOn (.fireAt) (Map.elems due), remaining)

nextWake :: Map CallbackId Callback -> Maybe UTCTime
nextWake = foldr (\value earliest -> Just (maybe value.fireAt (min value.fireAt) earliest)) Nothing

-- Claims commit before dispatch. A process crash may lose a claimed callback.
fireDue :: CallbackStore -> (Callback -> IO ()) -> IO ()
fireDue store fire = do
  now <- getCurrentTime
  due <- modifyStore store $ \current -> let (ready, rest) = dueCallbacks now current in (rest, ready)
  failures <- traverse (\value -> (Nothing <$ fire value) `catchSync` (pure . Just)) due
  case catMaybes failures of
    exception : _ -> throwIO exception
    [] -> pure ()

runScheduler :: Logger -> CallbackStore -> (Callback -> IO ()) -> IO ()
runScheduler logger store fire = Workers.withWorkers $ \workers ->
  forever $ (fireDue store (\callback -> Workers.submit workers (case callback.session of Named sid -> Just sid; Isolated -> Nothing) (fire callback `catchSync` report)) >> waitNext) `catchSync` \e -> report e >> threadDelay 5000000
  where
    report :: SomeException -> IO ()
    report e = logError logger "Scheduler failed" [("error", T.pack (show e))]
    waitNext = do
      version <- readTVarIO store.version
      upcoming <- nextWake <$> readMVar store.callbacks
      now <- getCurrentTime
      timer <- traverse (registerDelay . floor . (* 1000000) . max 0 . min 3600 . (`diffUTCTime` now)) upcoming
      atomically $ do
        changed <- (/= version) <$> readTVar store.version
        elapsed <- maybe (pure False) readTVar timer
        check (changed || elapsed)
