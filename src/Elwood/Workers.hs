-- | Scoped background work with FIFO admission for named lanes.
module Elwood.Workers (Workers, withWorkers, submit) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, mask, throwIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data Pool = Pool {next :: Int, jobs :: Map.Map Int (Async ()), lanes :: Map.Map Text (MVar ())}

newtype Workers = Workers (MVar (Maybe Pool))

withWorkers :: (Workers -> IO a) -> IO a
withWorkers = bracket (Workers <$> newMVar (Just (Pool 0 Map.empty Map.empty))) $ \(Workers state) -> do
  jobs <- modifyMVar state (\current -> pure (Nothing, maybe [] (Map.elems . (.jobs)) current))
  mapM_ cancel jobs

-- Reserve a lane and register its child within the same MVar ownership boundary.
-- Cancellation while waiting for admission cannot leave an orphaned queue gate.
submit :: Workers -> Maybe Text -> IO () -> IO ()
submit (Workers state) lane action = mask $ \restore -> modifyMVar_ state $ \case
  Nothing -> throwIO (userError "Worker scope is closed")
  Just pool -> do
    done <- newEmptyMVar
    let predecessor = lane >>= (`Map.lookup` pool.lanes)
        release = do
          putMVar done ()
          modifyMVar_ state $
            pure
              . fmap
                ( \current ->
                    current
                      { jobs = Map.delete pool.next current.jobs,
                        lanes = case lane of
                          Just key | Map.lookup key current.lanes == Just done -> Map.delete key current.lanes
                          _ -> current.lanes
                      }
                )
    job <- async (restore (mapM_ readMVar predecessor >> action) `finally` release)
    pure
      ( Just
          pool
            { next = pool.next + 1,
              jobs = Map.insert pool.next job pool.jobs,
              lanes = maybe pool.lanes (\key -> Map.insert key done pool.lanes) lane
            }
      )
