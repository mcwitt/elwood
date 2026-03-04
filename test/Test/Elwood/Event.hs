module Test.Elwood.Event (tests) where

import Elwood.Event (sessionToConversationId)
import Elwood.Event.Types (SessionConfig (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Event"
    [sessionIdTests]

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
