module Test.Elwood.Tools.AsyncTask (tests) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.AgentLoop (AgentResult (..))
import Elwood.Tools.AsyncTask (TaskId (..), insertTask, mkCancelTaskTool, mkCheckTaskTool, newAsyncTaskStore)
import Elwood.Tools.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.AsyncTask"
    [ checkTaskParsingTests,
      checkTaskRoundTripTests,
      cancelTaskParsingTests,
      cancelTaskRoundTripTests,
      ttlSweepTests
    ]

checkTaskParsingTests :: TestTree
checkTaskParsingTests =
  testGroup
    "check_task input parsing"
    [ testCase "empty input lists tasks" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object [])
        result @?= ToolSuccess "No async tasks.",
      testCase "non-object input returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (String "not an object")
        result @?= ToolError "Expected object input",
      testCase "non-string task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= (42 :: Int)])
        result @?= ToolError "Invalid 'task_id' parameter (must be a string)",
      testCase "empty task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("  " :: Text)])
        result @?= ToolError "task_id must not be empty",
      testCase "non-integer timeout returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["timeout" .= ("five" :: Text)])
        result @?= ToolError "Invalid 'timeout' parameter (must be an integer)",
      testCase "negative timeout returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["timeout" .= (-1 :: Int)])
        result @?= ToolError "timeout must be non-negative",
      testCase "unknown task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("nonexistent-id" :: Text)])
        result @?= ToolError "Unknown task: nonexistent-id"
    ]

checkTaskRoundTripTests :: TestTree
checkTaskRoundTripTests =
  testGroup
    "check_task round-trip"
    [ testCase "poll completed task returns result" $ do
        store <- newAsyncTaskStore 3600
        a <- Async.async $ pure $ AgentSuccess "task output" []
        _ <- Async.wait a -- ensure it's done
        insertTask store (TaskId "test-id") "my label" a
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("test-id" :: Text)])
        result @?= ToolSuccess "task output",
      testCase "poll completed task with error returns error" $ do
        store <- newAsyncTaskStore 3600
        a <- Async.async $ pure $ AgentError "something went wrong"
        _ <- Async.wait a
        insertTask store (TaskId "err-id") "failing task" a
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("err-id" :: Text)])
        result @?= ToolError "something went wrong",
      testCase "finished task is consumed on first check" $ do
        store <- newAsyncTaskStore 3600
        a <- Async.async $ pure $ AgentSuccess "one-time result" []
        _ <- Async.wait a
        insertTask store (TaskId "re-id") "consume-once" a
        let tool = mkCheckTaskTool store
        r1 <- tool.execute (object ["task_id" .= ("re-id" :: Text)])
        r1 @?= ToolSuccess "one-time result"
        r2 <- tool.execute (object ["task_id" .= ("re-id" :: Text)])
        r2 @?= ToolError "Unknown task: re-id",
      testCase "poll running task returns still running" $ do
        store <- newAsyncTaskStore 3600
        block <- newEmptyMVar
        blocker <- Async.async $ takeMVar block >> pure (AgentSuccess "never" [])
        insertTask store (TaskId "run-id") "running task" blocker
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("run-id" :: Text)])
        assertBool "should indicate running" $
          case result of
            ToolSuccess t -> T.isInfixOf "still running" t
            _ -> False
        Async.cancel blocker,
      testCase "list shows completed and running tasks" $ do
        store <- newAsyncTaskStore 3600
        done <- Async.async $ pure $ AgentSuccess "done" []
        _ <- Async.wait done
        blockR <- newEmptyMVar
        running <- Async.async $ takeMVar blockR >> pure (AgentSuccess "x" [])
        insertTask store (TaskId "done-id") "done task" done
        insertTask store (TaskId "run-id") "running task" running
        let tool = mkCheckTaskTool store
        result <- tool.execute (object [])
        case result of
          ToolSuccess t -> do
            assertBool "should contain done-id" (T.isInfixOf "done-id" t)
            assertBool "should contain run-id" (T.isInfixOf "run-id" t)
            assertBool "should contain completed" (T.isInfixOf "completed" t)
            assertBool "should contain running" (T.isInfixOf "running" t)
          _ -> assertFailure "expected ToolSuccess"
        Async.cancel running,
      testCase "poll with timeout waits for completion" $ do
        store <- newAsyncTaskStore 3600
        a <- Async.async $ pure $ AgentSuccess "waited result" []
        insertTask store (TaskId "wait-id") "wait task" a
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("wait-id" :: Text), "timeout" .= (5 :: Int)])
        result @?= ToolSuccess "waited result"
    ]

cancelTaskParsingTests :: TestTree
cancelTaskParsingTests =
  testGroup
    "cancel_task input parsing"
    [ testCase "missing task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCancelTaskTool store
        result <- tool.execute (object [])
        result @?= ToolError "Missing required 'task_id' parameter",
      testCase "empty task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCancelTaskTool store
        result <- tool.execute (object ["task_id" .= ("  " :: Text)])
        result @?= ToolError "task_id must not be empty",
      testCase "non-string task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCancelTaskTool store
        result <- tool.execute (object ["task_id" .= (42 :: Int)])
        result @?= ToolError "Invalid 'task_id' parameter (must be a string)",
      testCase "unknown task_id returns error" $ do
        store <- newAsyncTaskStore 3600
        let tool = mkCancelTaskTool store
        result <- tool.execute (object ["task_id" .= ("nonexistent" :: Text)])
        result @?= ToolError "Unknown task: nonexistent"
    ]

cancelTaskRoundTripTests :: TestTree
cancelTaskRoundTripTests =
  testGroup
    "cancel_task round-trip"
    [ testCase "cancel cancels and removes task" $ do
        store <- newAsyncTaskStore 3600
        block <- newEmptyMVar
        blocker <- Async.async $ takeMVar block >> pure (AgentSuccess "never" [])
        insertTask store (TaskId "cancel-id") "cancellable" blocker
        let cancelTool = mkCancelTaskTool store
        result <- cancelTool.execute (object ["task_id" .= ("cancel-id" :: Text)])
        assertBool "should confirm cancellation" $
          case result of
            ToolSuccess t -> T.isInfixOf "cancelled" t
            _ -> False
        -- Verify task is removed from store
        let checkTool = mkCheckTaskTool store
        checkResult <- checkTool.execute (object ["task_id" .= ("cancel-id" :: Text)])
        checkResult @?= ToolError "Unknown task: cancel-id"
    ]

ttlSweepTests :: TestTree
ttlSweepTests =
  testGroup
    "TTL sweep"
    [ testCase "expired task is swept on check_task" $ do
        -- TTL of 0 means everything expires immediately
        store <- newAsyncTaskStore 0
        a <- Async.async $ pure $ AgentSuccess "ephemeral" []
        _ <- Async.wait a
        insertTask store (TaskId "sweep-id") "sweepable" a
        -- Any check_task call triggers a sweep
        let tool = mkCheckTaskTool store
        result <- tool.execute (object ["task_id" .= ("sweep-id" :: Text)])
        result @?= ToolError "Unknown task: sweep-id",
      testCase "expired tasks disappear from listing" $ do
        store <- newAsyncTaskStore 0
        a <- Async.async $ pure $ AgentSuccess "gone" []
        _ <- Async.wait a
        insertTask store (TaskId "gone-id") "gone task" a
        let tool = mkCheckTaskTool store
        result <- tool.execute (object [])
        result @?= ToolSuccess "No async tasks."
    ]
