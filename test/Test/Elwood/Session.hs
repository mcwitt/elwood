module Test.Elwood.Session (tests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
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
      testCase "cancelSession returns False for unknown session" $ do
        locks <- newSessionLocks
        result <- cancelSession locks "nonexistent"
        result @?= False,
      testCase "idle session cannot be cancelled" $ do
        locks <- newSessionLocks
        withSessionLock locks "s1" $ pure ()
        cancelSession locks "s1" >>= (@?= False),
      testCase "cancellation belongs to an active turn" $ do
        locks <- newSessionLocks
        old <- withSessionLock locks "s1" $ do
          Just flag <- sessionCancelFlag locks "s1"
          cancelSession locks "s1" >>= (@?= True)
          readTVarIO flag >>= (@?= True)
          pure flag
        withSessionLock locks "s1" $ do
          Just fresh <- sessionCancelFlag locks "s1"
          atomically (writeTVar old True)
          readTVarIO fresh >>= (@?= False),
      testCase "sessionCancelFlag returns Nothing for unknown session" $ do
        locks <- newSessionLocks
        mFlag <- sessionCancelFlag locks "nonexistent"
        assertBool "expected Nothing" (null mFlag)
    ]
