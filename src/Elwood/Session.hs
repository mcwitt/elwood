module Elwood.Session (SessionLocks, newSessionLocks, withSessionLock, cancelSession, sessionCancelFlag) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data Session = Session {mutex :: MVar (), active :: TVar (Maybe (TVar Bool))}

newtype SessionLocks = SessionLocks (MVar (Map.Map Text Session))

newSessionLocks :: IO SessionLocks
newSessionLocks = SessionLocks <$> newMVar Map.empty

withSessionLock :: SessionLocks -> Text -> IO a -> IO a
withSessionLock (SessionLocks sessions) sid action = do
  session <- modifyMVar sessions $ \current -> case Map.lookup sid current of
    Just existing -> pure (current, existing)
    Nothing -> do
      fresh <- Session <$> newMVar () <*> newTVarIO Nothing
      pure (Map.insert sid fresh current, fresh)
  withMVar session.mutex $ \() ->
    bracket
      ( atomically $ do
          token <- newTVar False
          writeTVar session.active (Just token)
      )
      (\() -> atomically $ writeTVar session.active Nothing)
      (\() -> action)

cancelSession :: SessionLocks -> Text -> IO Bool
cancelSession sessions sid = do
  token <- sessionCancelFlag sessions sid
  atomically $ maybe (pure False) (\flag -> writeTVar flag True >> pure True) token

sessionCancelFlag :: SessionLocks -> Text -> IO (Maybe (TVar Bool))
sessionCancelFlag (SessionLocks sessions) sid = withMVar sessions $ \current ->
  maybe (pure Nothing) (readTVarIO . (.active)) (Map.lookup sid current)
