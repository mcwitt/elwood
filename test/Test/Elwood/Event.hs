module Test.Elwood.Event (tests) where

import Data.Text qualified as T
import Elwood.Event (sessionToConversationId)
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Event"
    [ sessionConfigTests,
      deliveryTargetTests,
      eventSourceTests,
      sessionIdTests
    ]

sessionConfigTests :: TestTree
sessionConfigTests =
  testGroup
    "SessionConfig"
    [ testCase "Isolated is equal to Isolated" $
        Isolated == Isolated @?= True,
      testCase "Named with same name is equal" $
        Named "foo" == Named "foo" @?= True,
      testCase "Named with different names is not equal" $
        Named "foo" == Named "bar" @?= False,
      testCase "Isolated is not equal to Named" $
        Isolated == Named "anything" @?= False
    ]

deliveryTargetTests :: TestTree
deliveryTargetTests =
  testGroup
    "DeliveryTarget"
    [ testCase "TelegramDelivery holds chat ID" $
        case TelegramDelivery 12345 of
          TelegramDelivery cid -> cid @?= 12345
          _ -> assertFailure "Expected TelegramDelivery",
      testCase "TelegramBroadcast equality" $
        TelegramBroadcast == TelegramBroadcast @?= True,
      testCase "TelegramReply equality" $
        TelegramReply == TelegramReply @?= True,
      testCase "LogOnly equality" $
        LogOnly == LogOnly @?= True,
      testCase "different targets are not equal" $
        TelegramBroadcast == LogOnly @?= False
    ]

eventSourceTests :: TestTree
eventSourceTests =
  testGroup
    "EventSource"
    [ testCase "WebhookSource holds name" $
        case WebhookSource "doorbell" of
          WebhookSource name -> name @?= "doorbell"
          _ -> assertFailure "Expected WebhookSource",
      testCase "CronSource holds name" $
        case CronSource "daily" of
          CronSource name -> name @?= "daily"
          _ -> assertFailure "Expected CronSource",
      testCase "TelegramSource holds chat ID" $
        case TelegramSource 12345 of
          TelegramSource cid -> cid @?= 12345
          _ -> assertFailure "Expected TelegramSource"
    ]

sessionIdTests :: TestTree
sessionIdTests =
  testGroup
    "sessionToConversationId"
    [ testCase "Isolated returns fixed ID" $ do
        let id1 = sessionToConversationId Isolated (WebhookSource "test")
        let id2 = sessionToConversationId Isolated (CronSource "job")
        -- Both should return the same fixed ID for isolated sessions
        id1 @?= id2,
      testCase "Isolated returns negative ID" $ do
        let convId = sessionToConversationId Isolated (WebhookSource "test")
        convId < 0 @?= True,
      testCase "Named with TelegramSource uses chat ID" $ do
        let convId = sessionToConversationId (Named "chat") (TelegramSource 12345)
        convId @?= 12345,
      testCase "Named with WebhookSource returns negative ID" $ do
        let convId = sessionToConversationId (Named "session") (WebhookSource "hook")
        convId < 0 @?= True,
      testCase "Named sessions with same name produce same ID" $ do
        let id1 = sessionToConversationId (Named "mysession") (WebhookSource "hook1")
        let id2 = sessionToConversationId (Named "mysession") (WebhookSource "hook2")
        id1 @?= id2,
      testCase "Named sessions with different names produce different IDs" $ do
        let id1 = sessionToConversationId (Named "session1") (WebhookSource "hook")
        let id2 = sessionToConversationId (Named "session2") (WebhookSource "hook")
        id1 /= id2 @?= True,
      testProperty "Named session IDs are deterministic" $
        \(name :: String) ->
          let t = T.pack name
              id1 = sessionToConversationId (Named t) (WebhookSource "test")
              id2 = sessionToConversationId (Named t) (WebhookSource "test")
           in id1 == id2
    ]
