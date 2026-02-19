module Test.Elwood.Event (tests) where

import Elwood.Event (sessionToConversationId)
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Test.Tasty
import Test.Tasty.HUnit

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
    [ testCase "TelegramDelivery holds session" $
        case TelegramDelivery "12345" of
          TelegramDelivery session -> session @?= "12345"
          _ -> assertFailure "Expected TelegramDelivery",
      testCase "TelegramBroadcast equality" $
        TelegramBroadcast == TelegramBroadcast @?= True,
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
