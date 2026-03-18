module Elwood.Session
  ( SessionLocks,
    newSessionLocks,
    withSessionLock,
    cancelSession,
    sessionCancelFlag,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Per-session state: a mutex for serializing turns plus a cancel flag.
data SessionState = SessionState
  { lock :: MVar (),
    cancelled :: TVar Bool
  }

-- | Per-session locks and cancel flags for concurrent event handling.
-- The outer TVar provides safe concurrent access to the session map;
-- each MVar acts as a mutex for its session.
newtype SessionLocks = SessionLocks (TVar (Map Text SessionState))

-- | Create an empty 'SessionLocks'
newSessionLocks :: IO SessionLocks
newSessionLocks = SessionLocks <$> newTVarIO Map.empty

-- | Run an action while holding the lock for the given session ID.
-- Creates the session state on first use. Resets the cancel flag on acquire.
-- Exception-safe via bracket.
withSessionLock :: SessionLocks -> Text -> IO a -> IO a
withSessionLock locks sid action = do
  state <- getOrCreateState locks sid
  bracket_
    (takeMVar state.lock)
    (putMVar state.lock ())
    (atomically (writeTVar state.cancelled False) >> action)

-- | Signal a running turn for the given session to stop.
-- Returns True if the session exists (cancel flag was set), False otherwise.
cancelSession :: SessionLocks -> Text -> IO Bool
cancelSession (SessionLocks tvar) sid =
  atomically $ do
    m <- readTVar tvar
    case Map.lookup sid m of
      Nothing -> pure False
      Just state -> do
        writeTVar state.cancelled True
        pure True

-- | Get the cancel flag for a session, if the session state exists.
sessionCancelFlag :: SessionLocks -> Text -> IO (Maybe (TVar Bool))
sessionCancelFlag (SessionLocks tvar) sid =
  atomically $ do
    m <- readTVar tvar
    pure $ (.cancelled) <$> Map.lookup sid m

-- | Get or create session state for a given session ID.
getOrCreateState :: SessionLocks -> Text -> IO SessionState
getOrCreateState (SessionLocks tvar) sid = do
  existing <- atomically $ Map.lookup sid <$> readTVar tvar
  case existing of
    Just s -> pure s
    Nothing -> do
      mv <- newMVar ()
      cv <- newTVarIO False
      let s = SessionState mv cv
      atomically $ do
        m <- readTVar tvar
        case Map.lookup sid m of
          Just s' -> pure s' -- another thread beat us
          Nothing -> do
            writeTVar tvar (Map.insert sid s m)
            pure s
