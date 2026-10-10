module Test.Elwood.State (tests) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Data.Aeson (encodeFile, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Approval qualified as Approval
import Elwood.Claude.AgentLoop (AgentResult (..))
import Elwood.Claude.Conversation
import Elwood.Claude.Types
import Elwood.Positive (literal)
import Elwood.Session qualified as Session
import Elwood.Tools.AsyncTask qualified as Task
import Elwood.Tools.Types
import System.Directory (createDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "State contracts"
    [ testCase "append reloads disk before its first read" $ withSystemTempDirectory "conversation" $ \root -> do
        first <- newConversationStore root
        first.appendMessages "s" [message "first"] Nothing
        second <- newConversationStore root
        second.appendMessages "s" [message "second"] Nothing
        third <- newConversationStore root
        conversation <- third.getConversation "s"
        conversation.messages @?= [message "first", message "second"],
      testCase "distinct conversation names never share a file" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        store.appendMessages "a/b" [message "slash"] Nothing
        store.appendMessages "a_b" [message "underscore"] Nothing
        reloaded <- newConversationStore root
        assertMessages reloaded "a/b" [message "slash"]
        assertMessages reloaded "a_b" [message "underscore"],
      testCase "long ASCII and Unicode conversation names round-trip" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        let names = [T.replicate 130 "a", T.replicate 130 "界"]
        mapM_ (\name -> store.appendMessages name [message name] Nothing) names
        reloaded <- newConversationStore root
        mapM_ (\name -> assertMessages reloaded name [message name]) names,
      testCase "legacy migration checks identity and writes v2" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        let legacy = root </> "conversations" </> "old_session.json"
        encodeFile legacy (Conversation "old/session" [message "legacy"] epoch)
        assertMessages store "old/session" [message "legacy"]
        removeFile legacy
        reloaded <- newConversationStore root
        assertMessages reloaded "old/session" [message "legacy"],
      testCase "legacy filename aliases cannot load another conversation" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        encodeFile (root </> "conversations" </> "a_b.json") (Conversation "a/b" [message "private"] epoch)
        result <- try @SomeException (store.getConversation "a_b")
        either (const (pure ())) (const (assertFailure "loaded the wrong session")) result,
      testCase "concurrent appends are all persisted" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        Async.mapConcurrently_ (\_ -> store.appendMessages "s" [message "item"] Nothing) [1 .. 30 :: Int]
        reloaded <- newConversationStore root
        reloaded.getConversation "s" >>= (@?= 30) . length . (.messages),
      testCase "failed persistence does not publish memory" $ withSystemTempDirectory "conversation" $ \root -> do
        store <- newConversationStore root
        _ <- store.getConversation "s"
        createDirectory (root </> "conversations" </> "v2" </> "73.json")
        result <- try @SomeException (store.appendMessages "s" [message "lost"] Nothing)
        either (const (pure ())) (const (assertFailure "write should fail")) result
        assertMessages store "s" [],
      testCase "approval cancellation removes pending request" $ do
        coordinator <- Approval.newApprovalCoordinator
        registered <- newEmptyMVar
        Async.withAsync (Approval.withApproval coordinator (literal @60) (putMVar registered)) $ \worker -> do
          key <- takeMVar registered
          Async.cancel worker
          Approval.respondToApproval coordinator key Approval.Approved >>= (@?= False),
      testCase "turn cancellation releases approval and queued successor" $ do
        coordinator <- Approval.newApprovalCoordinator
        sessions <- Session.newSessionLocks
        registered <- newEmptyMVar
        let active = Session.withSessionLock sessions "s" $ do
              token <- Session.sessionCancelFlag sessions "s"
              Approval.withCancellableApproval token coordinator (literal @60) (putMVar registered)
        Async.withAsync active $ \first -> do
          key <- takeMVar registered
          Async.withAsync (Session.withSessionLock sessions "s" (pure ())) $ \second -> do
            Session.cancelSession sessions "s" >>= (@?= True)
            timeout 1000000 (Async.wait first) >>= (@?= Just Approval.Denied)
            timeout 1000000 (Async.wait second) >>= (@?= Just ())
            Approval.respondToApproval coordinator key Approval.Approved >>= (@?= False),
      testCase "approval response is single use" $ do
        coordinator <- Approval.newApprovalCoordinator
        registered <- newEmptyMVar
        Async.withAsync (Approval.withApproval coordinator (literal @60) (putMVar registered)) $ \worker -> do
          key <- takeMVar registered
          Approval.respondToApproval coordinator key Approval.Approved >>= (@?= True)
          Approval.respondToApproval coordinator key Approval.Denied >>= (@?= False)
          Async.wait worker >>= (@?= Approval.Approved),
      testCase "running tasks survive retention expiry" $ do
        store <- Task.newAsyncTaskStore 0
        gate <- newEmptyMVar
        Task.startTask store (Task.TaskId "running") "running" (takeMVar gate >> pure (AgentSuccess "done" []))
        result <- (Task.mkCheckTaskTool store).execute (object ["task_id" .= ("running" :: Text)])
        case result of
          ToolSuccess _ -> pure ()
          _ -> assertFailure (show result)
        Task.closeTaskStore store,
      testCase "task shutdown closes admission" $ do
        store <- Task.newAsyncTaskStore 60
        Task.closeTaskStore store
        result <- timeout 1000000 (try @SomeException (Task.startTask store (Task.TaskId "late") "late" (pure (AgentSuccess "done" []))))
        case result of
          Just (Left _) -> pure ()
          _ -> assertFailure "closed store accepted a task or blocked",
      testCase "old turn cancellation preserves successor tasks" $ do
        store <- Task.newAsyncTaskStore 60
        old <- newTVarIO False
        fresh <- newTVarIO False
        Task.startTask (Task.forTurn fresh store) (Task.TaskId "fresh") "fresh" (pure (AgentSuccess "fresh" []))
        atomically (writeTVar old True)
        Task.cancelTurn old store >>= (@?= 0)
        denied <- try @SomeException (Task.startTask (Task.forTurn old store) (Task.TaskId "late") "late" (pure (AgentSuccess "late" [])))
        either (const (pure ())) (const (assertFailure "cancelled turn admitted a task")) denied
        (Task.mkAwaitTaskTool store).execute (object ["task_id" .= ("fresh" :: Text)]) >>= (@?= toolSuccess "fresh")
        Task.closeTaskStore store,
      testCase "task cancellation is scoped to its owner" $ do
        store <- Task.newAsyncTaskStore 60
        let first = Task.scopeTasks "first" store
            second = Task.scopeTasks "second" store
        Task.startTask first (Task.TaskId "a") "first" (pure (AgentSuccess "a" []))
        Task.startTask second (Task.TaskId "b") "second" (pure (AgentSuccess "b" []))
        Task.cancelAllTasks first >>= (@?= 1)
        (Task.mkAwaitTaskTool second).execute (object ["task_id" .= ("b" :: Text)]) >>= (@?= toolSuccess "b")
        Task.closeTaskStore store
    ]
  where
    message text = ClaudeMessage User [TextBlock text]

assertMessages :: ConversationStore -> Text -> [ClaudeMessage] -> Assertion
assertMessages store sid expected = store.getConversation sid >>= \conversation -> conversation.messages @?= expected
