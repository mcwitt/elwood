module Test.Elwood.Event (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (bracket, finally)
import Control.Monad (replicateM)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Elwood.AgentSettings qualified as Profile (AgentProfile (..))
import Elwood.AgentSettings qualified as Settings
import Elwood.App (fireCallback)
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (newPruneHorizons)
import Elwood.Event (AppEnv (..), DeliveryPolicy (..), Event (..), attachPromptNote, handleEventWith, sessionToConversationId)
import Elwood.Event.Types (DeliveryTarget (..), EventSource (..), SavedAttachment (..), SessionConfig (..))
import Elwood.Logging (LogLevel (..), LogMsg (..))
import Elwood.Metrics (newMetricsStore)
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Scheduler (newCallbackStore)
import Elwood.Scheduler qualified as Scheduler
import Elwood.Session (newSessionLocks)
import Elwood.Telegram.Client qualified as Telegram
import Elwood.Tools qualified as Tools
import Elwood.Tools.AsyncTask (closeTaskStore)
import Elwood.Workers qualified as Workers
import Network.HTTP.Types.Status (status200)
import Network.Wai (pathInfo, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Event"
    [sessionIdTests, attachPromptNoteTests, deliveryAndSessionJson, workerTests, deliveryTests, callbackFailureTests]

deliveryAndSessionJson :: TestTree
deliveryAndSessionJson =
  testGroup
    "SessionConfig/DeliveryTarget JSON round-trip"
    [ testCase "Isolated" $
        decode (encode Isolated) @?= Just Isolated,
      testCase "Named" $
        decode (encode (Named "chat-123")) @?= Just (Named "chat-123"),
      testCase "TelegramDelivery" $
        decode (encode (TelegramDelivery (1 :| [2, 3]))) @?= Just (TelegramDelivery (1 :| [2, 3])),
      testCase "TelegramBroadcast" $
        decode (encode TelegramBroadcast) @?= Just TelegramBroadcast,
      testCase "LogOnly" $
        decode (encode LogOnly) @?= Just LogOnly
    ]

sessionIdTests :: TestTree
sessionIdTests =
  testGroup
    "sessionToConversationId"
    [ testCase "Isolated returns Nothing" $
        sessionToConversationId Isolated @?= Nothing,
      testCase "Named returns Just with session name" $
        sessionToConversationId (Named "my-session") @?= Just "my-session",
      testCase "Named with numeric name passes through as-is" $
        sessionToConversationId (Named "12345") @?= Just "12345",
      testCase "Named sessions with different names return different IDs" $ do
        let id1 = sessionToConversationId (Named "session1")
        let id2 = sessionToConversationId (Named "session2")
        id1 /= id2 @?= True
    ]

photoAtt :: SavedAttachment
photoAtt =
  SavedAttachment {path = "inbox/1-a.jpg", mediaType = "image/jpeg", sizeBytes = 250880, perceivable = True, originalName = Nothing}

pdfAtt :: SavedAttachment
pdfAtt =
  SavedAttachment {path = "inbox/1-b.pdf", mediaType = "application/pdf", sizeBytes = 1258291, perceivable = False, originalName = Just "report.pdf"}

attachPromptNoteTests :: TestTree
attachPromptNoteTests =
  testGroup
    "attachPromptNote"
    [ testCase "empty list leaves prompt unchanged" $
        attachPromptNote "hello" [] @?= "hello",
      testCase "attachment notes retain meaningful metadata" $ do
        let note = attachPromptNote "save this" [photoAtt, pdfAtt]
        mapM_
          (\part -> assertBool (show part) (part `T.isInfixOf` note))
          ["save this", "inbox/1-a.jpg", "image/jpeg", "inbox/1-b.pdf", "application/pdf", "report.pdf"]
    ]

workerTests :: TestTree
workerTests =
  testGroup
    "worker scope"
    [ testCase "interrupted contending submissions do not strand the lane" $ Workers.withWorkers $ \workers -> do
        submitters <- replicateM 200 (async (Workers.submit workers (Just "shared") (pure ())))
        mapM_ cancel submitters
        finished <- newEmptyMVar
        Workers.submit workers (Just "shared") (putMVar finished ())
        timeout 2000000 (takeMVar finished) >>= (@?= Just ()),
      testCase "named jobs execute in admission order while other lanes proceed" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        complete <- newEmptyMVar
        other <- newEmptyMVar
        order <- newIORef ([] :: [Int])
        Workers.withWorkers $ \workers -> do
          Workers.submit workers (Just "one") (putMVar started () >> takeMVar release >> modifyIORef' order (<> [1]))
          takeMVar started
          Workers.submit workers (Just "one") (modifyIORef' order (<> [2]) >> putMVar complete ())
          Workers.submit workers (Just "other") (putMVar other ())
          takeMVar other
          readIORef order >>= (@?= [])
          putMVar release ()
          takeMVar complete
          readIORef order >>= (@?= [1, 2]),
      testCase "leaving the scope cancels and joins jobs" $ do
        started <- newEmptyMVar
        block <- newEmptyMVar
        stopped <- newEmptyMVar
        Workers.withWorkers $ \workers -> do
          Workers.submit workers Nothing ((putMVar started () >> takeMVar block) `finally` putMVar stopped ())
          takeMVar started
        takeMVar stopped
    ]

deliveryTests :: TestTree
deliveryTests =
  testGroup
    "turn delivery contract"
    [ testCase "suppression retains history and allows explicit send_message" $ withDeliveryFixture $ \env event delivered -> do
        handleEventWith env (SuppressContaining "SKIP") event >>= (@?= Right "SKIP")
        texts <- readIORef delivered
        length texts @?= 1
        assertBool "explicit message reached the transport" ("explicit" `T.isInfixOf` T.concat texts)
        history <- env.conversations.getConversation "session"
        length history.messages @?= 4,
      testCase "buffered delivery preserves ordinary output order when not suppressed" $ withDeliveryFixture $ \env event delivered -> do
        handleEventWith env (SuppressContaining "absent") event >>= (@?= Right "SKIP")
        texts <- readIORef delivered
        length texts @?= 3
        map T.strip texts @?= ["explicit", "narration", "SKIP"]
    ]
  where
    withDeliveryFixture action = withSystemTempDirectory "event-delivery" $ \directory -> do
      count <- newIORef (0 :: Int)
      delivered <- newIORef []
      let text value = object ["type" .= ("text" :: T.Text), "text" .= (value :: T.Text)]
          application request respond = do
            result <- case pathInfo request of
              ["v1", "messages"] -> do
                iteration <- atomicModifyIORef' count (\n -> (n + 1, n))
                pure $
                  object
                    [ "id" .= ("reply" :: T.Text),
                      "stop_reason" .= (if iteration == 0 then "tool_use" else "end_turn" :: T.Text),
                      "usage" .= object ["input_tokens" .= (1 :: Int), "output_tokens" .= (1 :: Int)],
                      "content" .= if iteration == 0 then [text "narration", object ["type" .= ("tool_use" :: T.Text), "id" .= ("call" :: T.Text), "name" .= ("send_message" :: T.Text), "input" .= object ["message" .= ("explicit" :: T.Text)]]] else [text "SKIP"]
                    ]
              ["sendMessage"] -> do
                body <- strictRequestBody request
                case decode body of
                  Just (Object fields) | Just (String value) <- KM.lookup "text" fields -> modifyIORef' delivered (<> [value])
                  _ -> assertFailure "invalid outgoing message"
                pure (object ["ok" .= True, "result" .= object ["message_id" .= (1 :: Int)]])
              _ -> pure (object ["ok" .= True, "result" .= True])
            respond (responseLBS status200 [] (encode result))
      testWithApplication (pure application) $ \port -> do
        let logger = LogAction (const (pure ()))
            url = "http://127.0.0.1:" <> show port
        telegram <- Telegram.newClient logger "token"
        claude <- Claude.newClient (Map.singleton "mock" (ProviderConfig "mock" (T.pack url) Nothing AnthropicFormat ImagesEmbedded))
        conversations <- Claude.newInMemoryConversationStore
        metrics <- newMetricsStore
        horizons <- newPruneHorizons
        sessions <- newSessionLocks
        toggles <- newTVarIO Map.empty
        callbacks <- newCallbackStore logger directory
        now <- getCurrentTime
        bracket (Tools.newAsyncTaskStore 3600) closeTaskStore $ \tasks -> do
          let profile = (Settings.resolveProfile mempty) {Profile.model = Settings.ModelRef "mock" "model", Profile.systemPrompt = []}
              env =
                AppEnv
                  logger
                  telegram {Telegram.tcBaseUrl = url}
                  claude
                  conversations
                  Tools.newToolRegistry
                  Tools.noApprovalChannel
                  Nothing
                  Nothing
                  directory
                  profile
                  Map.empty
                  metrics
                  horizons
                  sessions
                  False
                  toggles
                  (Settings.AgentPreset Nothing mempty)
                  Map.empty
                  []
                  Nothing
                  tasks
                  callbacks
              event = Event (WebhookSource "test") now Null "hello" Nothing [] (Named "session") (TelegramDelivery (123 :| []))
          action env event delivered

callbackFailureTests :: TestTree
callbackFailureTests = testCase "scheduled jobs report domain and persistence failures after admission" $ do
  entries <- newIORef []
  now <- getCurrentTime
  let logger = LogAction (\entry -> modifyIORef' entries (<> [entry]))
      callback = Scheduler.Callback (Scheduler.CallbackId "callback") now "wake" (Named "session") LogOnly now
  fireCallback logger (const (pure (Left "provider unavailable"))) callback
  fireCallback logger (const (ioError (userError "disk unavailable"))) callback
  logged <- readIORef entries
  length logged @?= 2
  map (.level) logged @?= [Error, Error]
  assertBool "domain failure visible" (any ((== Just "provider unavailable") . lookup "error" . (.context)) logged)
  assertBool "persistence failure visible" (any (maybe False (T.isInfixOf "disk unavailable") . lookup "error" . (.context)) logged)
