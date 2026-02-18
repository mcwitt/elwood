module Test.Elwood.Claude.Types (tests) where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Elwood.Claude.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.Types"
    [ roleTests,
      contentBlockTests,
      claudeMessageTests,
      roundTripTests
    ]

roleTests :: TestTree
roleTests =
  testGroup
    "Role"
    [ testCase "User encodes to 'user'" $
        encode User @?= "\"user\"",
      testCase "Assistant encodes to 'assistant'" $
        encode Assistant @?= "\"assistant\"",
      testCase "decodes 'user' to User" $
        decode "\"user\"" @?= Just User,
      testCase "decodes 'assistant' to Assistant" $
        decode "\"assistant\"" @?= Just Assistant,
      testCase "fails on invalid role" $
        (decode "\"invalid\"" :: Maybe Role) @?= Nothing
    ]

contentBlockTests :: TestTree
contentBlockTests =
  testGroup
    "ContentBlock"
    [ testCase "TextBlock encodes correctly" $ do
        let block = TextBlock "Hello, world!"
            json = encode block
        -- Should have type: "text" and text field
        case decode json of
          Just obj -> obj @?= block
          Nothing -> assertFailure "Failed to decode TextBlock",
      testCase "ToolUseBlock encodes correctly" $ do
        let block = ToolUseBlock "toolu_123" "read_file" (Aeson.object ["path" Aeson..= ("test.txt" :: String)])
        case decode (encode block) of
          Just decoded -> decoded @?= block
          Nothing -> assertFailure "Failed to decode ToolUseBlock",
      testCase "ToolResultBlock encodes correctly" $ do
        let block = ToolResultBlock "toolu_123" "file contents here" False
        case decode (encode block) of
          Just decoded -> decoded @?= block
          Nothing -> assertFailure "Failed to decode ToolResultBlock",
      testCase "ToolResultBlock with error encodes is_error" $ do
        let block = ToolResultBlock "toolu_123" "Error: file not found" True
            json = encode block
        -- Check that is_error is present in the JSON by decoding and checking
        case decode json :: Maybe Value of
          Just (Object obj) -> KM.member "is_error" obj @?= True
          _ -> assertFailure "Expected JSON object"
    ]

claudeMessageTests :: TestTree
claudeMessageTests =
  testGroup
    "ClaudeMessage"
    [ testCase "user message encodes correctly" $ do
        let msg = ClaudeMessage User [TextBlock "Hello"]
        case decode (encode msg) of
          Just decoded -> decoded @?= msg
          Nothing -> assertFailure "Failed to decode ClaudeMessage",
      testCase "assistant message with multiple blocks" $ do
        let msg =
              ClaudeMessage
                Assistant
                [ TextBlock "Let me help",
                  ToolUseBlock "toolu_1" "read_file" (Aeson.object [])
                ]
        case decode (encode msg) of
          Just decoded -> decoded @?= msg
          Nothing -> assertFailure "Failed to decode multi-block message",
      testCase "empty content list encodes" $ do
        let msg = ClaudeMessage User []
        case decode (encode msg) of
          Just decoded -> decoded @?= msg
          Nothing -> assertFailure "Failed to decode empty message"
    ]

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "JSON round-trip"
    [ testCase "Conversation round-trips" $ do
        let conv =
              Conversation
                { convChatId = 12345,
                  convMessages =
                    [ ClaudeMessage User [TextBlock "Hi"],
                      ClaudeMessage Assistant [TextBlock "Hello!"]
                    ],
                  convLastUpdated = read "2024-01-01 00:00:00 UTC"
                }
        case decode (encode conv) of
          Just decoded -> do
            convChatId decoded @?= convChatId conv
            length (convMessages decoded) @?= length (convMessages conv)
          Nothing -> assertFailure "Failed to decode Conversation",
      testCase "ToolSchema encodes name and description" $ do
        let schema =
              ToolSchema
                { tsName = "my_tool",
                  tsDescription = "Does something useful",
                  tsInputSchema =
                    Aeson.object
                      [ "type" Aeson..= ("object" :: String),
                        "properties" Aeson..= Aeson.object []
                      ]
                }
            json = encode schema
        -- Check by decoding and verifying fields
        case decode json :: Maybe Value of
          Just (Object obj) -> do
            KM.member "name" obj @?= True
            KM.member "description" obj @?= True
          _ -> assertFailure "Expected JSON object"
    ]
