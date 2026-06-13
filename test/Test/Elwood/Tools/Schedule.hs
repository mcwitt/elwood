{-# LANGUAGE OverloadedRecordDot #-}

module Test.Elwood.Tools.Schedule (tests) where

import Data.Aeson (object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Logging (LogLevel (..), newLogger)
import Elwood.Scheduler (Callback (..), CallbackId (..), listCallbacks, newCallbackStore)
import Elwood.Tools.Schedule (mkCancelCallbackTool, mkListCallbacksTool, mkScheduleCallbackTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.Schedule"
    [ testCase "schedule_callback enqueues a callback" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let tool = mkScheduleCallbackTool lgr store (Named "chat-1") (TelegramDelivery (1 :| []))
          res <- tool.execute (object ["at" .= ("2030-01-01T09:00:00Z" :: String), "prompt" .= ("check X" :: String)])
          case res of
            ToolSuccess _ -> pure ()
            ToolError e -> assertFailure ("expected success, got: " <> show e)
          pending <- listCallbacks store
          length pending @?= 1,
      testCase "schedule_callback rejects a bad timestamp" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let tool = mkScheduleCallbackTool lgr store (Named "chat-1") (TelegramDelivery (1 :| []))
          res <- tool.execute (object ["at" .= ("not-a-time" :: String), "prompt" .= ("x" :: String)])
          case res of
            ToolError _ -> pure ()
            ToolSuccess _ -> assertFailure "expected an error for bad timestamp",
      testCase "schedule_callback requires prompt" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let tool = mkScheduleCallbackTool lgr store (Named "chat-1") (TelegramDelivery (1 :| []))
          res <- tool.execute (object ["at" .= ("2030-01-01T09:00:00Z" :: String)])
          case res of
            ToolError _ -> pure ()
            ToolSuccess _ -> assertFailure "expected an error for missing prompt",
      testCase "list_callbacks reports empty" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let tool = mkListCallbacksTool store
          res <- tool.execute (object [])
          case res of
            ToolSuccess _ -> pure ()
            ToolError e -> assertFailure ("expected success, got: " <> show e),
      testCase "cancel_callback cancels what schedule created" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let sTool = mkScheduleCallbackTool lgr store (Named "chat-1") (TelegramDelivery (1 :| []))
          _ <- sTool.execute (object ["at" .= ("2030-01-01T09:00:00Z" :: String), "prompt" .= ("y" :: String)])
          [cb] <- listCallbacks store
          let cTool = mkCancelCallbackTool store
          res <- cTool.execute (object ["id" .= cb.id_.unCallbackId])
          case res of
            ToolSuccess _ -> pure ()
            ToolError e -> assertFailure ("expected success, got: " <> show e)
          remaining <- listCallbacks store
          length remaining @?= 0,
      testCase "cancel_callback errors on unknown id" $
        withSystemTempDirectory "sched-tool" $ \dir -> do
          lgr <- newLogger Error
          store <- newCallbackStore lgr dir
          let cTool = mkCancelCallbackTool store
          res <- cTool.execute (object ["id" .= ("nope" :: String)])
          case res of
            ToolError _ -> pure ()
            ToolSuccess _ -> assertFailure "expected an error for unknown id"
    ]
