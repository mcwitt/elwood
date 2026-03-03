module Elwood.Session
  ( SessionLocks,
    newSessionLocks,
    withSessionLock,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Per-session locks for serializing concurrent event handling.
-- The outer TVar provides safe concurrent access to the lock map;
-- each MVar acts as a mutex for its session.
newtype SessionLocks = SessionLocks (TVar (Map Text (MVar ())))

-- | Create an empty 'SessionLocks'
newSessionLocks :: IO SessionLocks
newSessionLocks = SessionLocks <$> newTVarIO Map.empty

-- | Run an action while holding the lock for the given session ID.
-- Creates the lock on first use. Exception-safe via bracket.
withSessionLock :: SessionLocks -> Text -> IO a -> IO a
withSessionLock (SessionLocks tvar) sid action = do
  lock <- getOrCreateLock
  bracket_ (takeMVar lock) (putMVar lock ()) action
  where
    getOrCreateLock :: IO (MVar ())
    getOrCreateLock = do
      existing <- atomically $ Map.lookup sid <$> readTVar tvar
      case existing of
        Just mv -> pure mv
        Nothing -> do
          mv <- newMVar ()
          atomically $ do
            m <- readTVar tvar
            case Map.lookup sid m of
              Just mv' -> pure mv' -- another thread beat us
              Nothing -> do
                writeTVar tvar (Map.insert sid mv m)
                pure mv
