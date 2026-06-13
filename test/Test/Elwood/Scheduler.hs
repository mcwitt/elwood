{-# LANGUAGE OverloadedRecordDot #-}

module Test.Elwood.Scheduler (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson (decode, encode)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Logging (LogLevel (..), newLogger)
import Elwood.Scheduler
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Scheduler"
    [ pureHelpers,
      callbackJson,
      storeOps,
      firing
    ]

-- A fixed UTCTime at midnight of the given date.
mkUTC :: Integer -> Int -> Int -> UTCTime
mkUTC y m d = UTCTime (fromGregorian y m d) 0

mkCallback :: Text -> UTCTime -> Callback
mkCallback cid t =
  Callback
    { id_ = CallbackId cid,
      fireAt = t,
      prompt = "do the thing",
      session = Named "chat-1",
      deliveryTarget = TelegramDelivery (1 :| []),
      createdAt = mkUTC 2020 1 1
    }

pureHelpers :: TestTree
pureHelpers =
  testGroup
    "pure helpers"
    [ testCase "dueCallbacks splits past from future" $ do
        let now = mkUTC 2020 1 1
            past = mkCallback "past" (mkUTC 2000 1 1)
            future = mkCallback "future" (mkUTC 3000 1 1)
            m = Map.fromList [(past.id_, past), (future.id_, future)]
            (due, rest) = dueCallbacks now m
        map (.id_) due @?= [CallbackId "past"]
        Map.keys rest @?= [CallbackId "future"],
      testCase "nextWake of empty map is Nothing" $
        nextWake Map.empty @?= Nothing,
      testCase "nextWake picks the earliest fireAt" $ do
        let a = mkCallback "a" (mkUTC 2030 1 1)
            b = mkCallback "b" (mkUTC 2025 1 1)
            m = Map.fromList [(a.id_, a), (b.id_, b)]
        nextWake m @?= Just (mkUTC 2025 1 1)
    ]

callbackJson :: TestTree
callbackJson =
  testCase "Callback JSON round-trips" $ do
    let cb = mkCallback "abc" (mkUTC 2026 6 13)
    decode (encode cb) @?= Just cb

storeOps :: TestTree
storeOps =
  testGroup
    "store operations"
    [ testCase "schedule persists and reloads across new store" $
        withSystemTempDirectory "sched" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          _ <- scheduleCallback store (mkCallback "a" (mkUTC 3000 1 1))
          store2 <- newCallbackStore lgr dir
          loaded <- listCallbacks store2
          map (.id_) loaded @?= [CallbackId "a"],
      testCase "cancel removes and persists" $
        withSystemTempDirectory "sched" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          _ <- scheduleCallback store (mkCallback "a" (mkUTC 3000 1 1))
          _ <- scheduleCallback store (mkCallback "b" (mkUTC 3000 1 2))
          ok <- cancelCallback store (CallbackId "a")
          ok @?= True
          store2 <- newCallbackStore lgr dir
          loaded <- listCallbacks store2
          map (.id_) loaded @?= [CallbackId "b"],
      testCase "cancel of unknown id returns False" $
        withSystemTempDirectory "sched" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          ok <- cancelCallback store (CallbackId "nope")
          ok @?= False
    ]

firing :: TestTree
firing =
  testGroup
    "fireDue"
    [ testCase "fires due callbacks once and removes them from disk" $
        withSystemTempDirectory "sched" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          _ <- scheduleCallback store (mkCallback "past" (mkUTC 2000 1 1))
          _ <- scheduleCallback store (mkCallback "future" (mkUTC 3000 1 1))
          fired <- newTVarIO []
          fireDue store (\cb -> atomically (modifyTVar' fired (cb.id_ :)))
          firedIds <- readTVarIO fired
          firedIds @?= [CallbackId "past"]
          -- still-pending in memory
          remaining <- listCallbacks store
          map (.id_) remaining @?= [CallbackId "future"]
          -- and persisted: a fresh store does not re-fire "past"
          store2 <- newCallbackStore lgr dir
          reloaded <- listCallbacks store2
          map (.id_) reloaded @?= [CallbackId "future"]
    ]
