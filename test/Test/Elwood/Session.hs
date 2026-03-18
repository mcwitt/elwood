module Test.Elwood.Session (tests) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Elwood.Session (cancelSession, newSessionLocks, sessionCancelFlag, withSessionLock)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Session"
    [ testCase "single lock acquire and release" $ do
        locks <- newSessionLocks
        ref <- newIORef (0 :: Int)
        withSessionLock locks "s1" $ atomicModifyIORef' ref (\n -> (n + 1, ()))
        val <- readIORef ref
        val @?= 1,
      testCase "same session serializes concurrent actions" $ do
        locks <- newSessionLocks
        -- Record the order of execution: each action appends its start and end
        ref <- newIORef ([] :: [String])
        let append s = atomicModifyIORef' ref (\xs -> (xs ++ [s], ()))
        -- gate lets us ensure thread A is holding the lock before B starts waiting
        gate <- newEmptyMVar
        done <- newEmptyMVar
        _ <- forkIO $ withSessionLock locks "s1" $ do
          append "A-start"
          putMVar gate () -- signal that A holds the lock
          threadDelay 50000 -- hold lock for 50ms
          append "A-end"
        -- Wait until A holds the lock
        takeMVar gate
        _ <- forkIO $ do
          withSessionLock locks "s1" $ do
            append "B-start"
            append "B-end"
          putMVar done ()
        takeMVar done
        order <- readIORef ref
        order @?= ["A-start", "A-end", "B-start", "B-end"],
      testCase "different sessions run concurrently" $ do
        locks <- newSessionLocks
        -- Two different sessions should not block each other
        ref <- newIORef ([] :: [String])
        let append s = atomicModifyIORef' ref (\xs -> (xs ++ [s], ()))
        gate1 <- newEmptyMVar
        gate2 <- newEmptyMVar
        done1 <- newEmptyMVar
        done2 <- newEmptyMVar
        -- Thread A locks session "s1", signals, then waits for B to also be running
        _ <- forkIO $ withSessionLock locks "s1" $ do
          append "A-start"
          putMVar gate1 ()
          takeMVar gate2 -- wait for B to be running concurrently
          append "A-end"
          putMVar done1 ()
        -- Thread B locks session "s2", signals, then waits for A to also be running
        _ <- forkIO $ withSessionLock locks "s2" $ do
          append "B-start"
          putMVar gate2 ()
          takeMVar gate1 -- wait for A to be running concurrently
          append "B-end"
          putMVar done2 ()
        takeMVar done1
        takeMVar done2
        -- Both started before either ended -- proves they ran concurrently
        order <- readIORef ref
        assertBool
          ("expected interleaved execution, got: " ++ show order)
          (order `elem` [["A-start", "B-start", "A-end", "B-end"], ["B-start", "A-start", "B-end", "A-end"], ["A-start", "B-start", "B-end", "A-end"], ["B-start", "A-start", "A-end", "B-end"]]),
      testCase "lock is released after exception" $ do
        locks <- newSessionLocks
        -- First action throws
        _ <-
          try @SomeException $
            withSessionLock locks "s1" $
              error "boom"
        -- Second action should still acquire the lock (not deadlock)
        ref <- newTVarIO False
        withSessionLock locks "s1" $ atomically $ writeTVar ref True
        val <- readTVarIO ref
        val @?= True,
      testCase "reentrant access to same session deadlocks (documents behavior)" $ do
        -- This test documents that the lock is NOT reentrant.
        -- A nested withSessionLock on the same session will block forever.
        locks <- newSessionLocks
        result <- newEmptyMVar :: IO (MVar String)
        _ <- forkIO $ do
          withSessionLock locks "s1" $
            withSessionLock locks "s1" $
              putMVar result "completed"
          putMVar result "should not reach"
        -- Give it 100ms -- if it hasn't completed, it's deadlocked (expected)
        threadDelay 100000
        -- Try non-blocking take; should be empty because the thread is deadlocked
        val <- tryTakeMVarTimeout result
        val @?= Nothing,
      testCase "cancelSession returns False for unknown session" $ do
        locks <- newSessionLocks
        result <- cancelSession locks "nonexistent"
        result @?= False,
      testCase "cancelSession sets the cancel flag" $ do
        locks <- newSessionLocks
        -- Create session state by acquiring the lock
        withSessionLock locks "s1" $ pure ()
        -- Cancel it
        result <- cancelSession locks "s1"
        result @?= True
        -- Verify the flag is set
        mFlag <- sessionCancelFlag locks "s1"
        case mFlag of
          Nothing -> assertFailure "expected cancel flag to exist"
          Just tv -> do
            val <- readTVarIO tv
            val @?= True,
      testCase "withSessionLock resets cancel flag" $ do
        locks <- newSessionLocks
        -- Create session and cancel it
        withSessionLock locks "s1" $ pure ()
        _ <- cancelSession locks "s1"
        -- Acquire the lock again — should reset the flag
        withSessionLock locks "s1" $ do
          mFlag <- sessionCancelFlag locks "s1"
          case mFlag of
            Nothing -> assertFailure "expected cancel flag to exist"
            Just tv -> do
              val <- readTVarIO tv
              val @?= False,
      testCase "sessionCancelFlag returns Nothing for unknown session" $ do
        locks <- newSessionLocks
        mFlag <- sessionCancelFlag locks "nonexistent"
        assertBool "expected Nothing" (null mFlag)
    ]

-- | Try to take from an MVar, returning Nothing if empty
tryTakeMVarTimeout :: MVar a -> IO (Maybe a)
tryTakeMVarTimeout mv = do
  ref <- newIORef Nothing
  _ <- forkIO $ do
    v <- takeMVar mv
    atomicModifyIORef' ref (const (Just v, ()))
  threadDelay 10000 -- 10ms grace
  readIORef ref
