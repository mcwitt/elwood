module Test.Elwood.Claude.Types (tests) where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Elwood.Claude.Conversation (ConversationStore (..), newInMemoryConversationStore)
import Elwood.Claude.Types
import Elwood.Thinking (ThinkingEffort (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.Types"
    [ roleTests,
      contentBlockTests,
      claudeMessageTests,
      usageTests,
      roundTripTests,
      conversationCacheTests,
      outputConfigTests
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
        let block = ToolUseBlock (ToolUseId "toolu_123") "read_file" (Aeson.object ["path" Aeson..= ("test.txt" :: String)])
        case decode (encode block) of
          Just decoded -> decoded @?= block
          Nothing -> assertFailure "Failed to decode ToolUseBlock",
      testCase "ToolResultBlock encodes correctly" $ do
        let block = ToolResultBlock (ToolUseId "toolu_123") "file contents here" False
        case decode (encode block) of
          Just decoded -> decoded @?= block
          Nothing -> assertFailure "Failed to decode ToolResultBlock",
      testCase "ToolResultBlock with error encodes is_error" $ do
        let block = ToolResultBlock (ToolUseId "toolu_123") "Error: file not found" True
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
                  ToolUseBlock (ToolUseId "toolu_1") "read_file" (Aeson.object [])
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

usageTests :: TestTree
usageTests =
  testGroup
    "Usage"
    [ testCase "parses with cache fields present" $ do
        let json =
              Aeson.object
                [ "input_tokens" Aeson..= (100 :: Int),
                  "output_tokens" Aeson..= (50 :: Int),
                  "cache_creation_input_tokens" Aeson..= (10 :: Int),
                  "cache_read_input_tokens" Aeson..= (20 :: Int)
                ]
        case Aeson.fromJSON json :: Aeson.Result Usage of
          Aeson.Success usage -> do
            usage.inputTokens @?= 100
            usage.outputTokens @?= 50
            usage.cacheCreationInputTokens @?= 10
            usage.cacheReadInputTokens @?= 20
          Aeson.Error err -> assertFailure $ "Failed to parse Usage: " <> err,
      testCase "parses with cache fields absent (defaults to 0)" $ do
        let json =
              Aeson.object
                [ "input_tokens" Aeson..= (100 :: Int),
                  "output_tokens" Aeson..= (50 :: Int)
                ]
        case Aeson.fromJSON json :: Aeson.Result Usage of
          Aeson.Success usage -> do
            usage.inputTokens @?= 100
            usage.outputTokens @?= 50
            usage.cacheCreationInputTokens @?= 0
            usage.cacheReadInputTokens @?= 0
          Aeson.Error err -> assertFailure $ "Failed to parse Usage: " <> err
    ]

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "JSON round-trip"
    [ testCase "Conversation round-trips" $ do
        let conv =
              Conversation
                { sessionId = "12345",
                  messages =
                    [ ClaudeMessage User [TextBlock "Hi"],
                      ClaudeMessage Assistant [TextBlock "Hello!"]
                    ],
                  cacheExpiresAt = UTCTime (fromGregorian 2024 1 1) 0
                }
        case decode (encode conv) :: Maybe Conversation of
          Just decoded -> do
            decoded.sessionId @?= conv.sessionId
            length decoded.messages @?= length conv.messages
            decoded.cacheExpiresAt @?= conv.cacheExpiresAt
          Nothing -> assertFailure "Failed to decode Conversation",
      testCase "old JSON without cacheExpiresAt defaults to epoch" $ do
        let json =
              Aeson.object
                [ "sessionId" Aeson..= ("test" :: String),
                  "messages" Aeson..= ([] :: [Value]),
                  "lastUpdated" Aeson..= ("2024-01-01T00:00:00Z" :: String)
                ]
        case Aeson.fromJSON json :: Aeson.Result Conversation of
          Aeson.Success conv -> conv.cacheExpiresAt @?= epoch
          Aeson.Error err -> assertFailure $ "Failed to parse old Conversation JSON: " <> err,
      testCase "ToolSchema encodes name and description" $ do
        let schema =
              ToolSchema
                { name = "my_tool",
                  description = "Does something useful",
                  inputSchema =
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

conversationCacheTests :: TestTree
conversationCacheTests =
  testGroup
    "Conversation cache expiry"
    [ testCase "shorter TTL does not shrink cacheExpiresAt" $ do
        store <- newInMemoryConversationStore
        let sid :: Text = "test-session"
            msg = ClaudeMessage User [TextBlock "hello"]

        -- First append with 1h TTL
        store.appendMessages sid [msg] (Just CacheTtl1Hour)
        conv1 <- store.getConversation sid
        let expiry1 = conv1.cacheExpiresAt

        -- Second append with 5m TTL — must not shrink the expiry
        store.appendMessages sid [msg] (Just CacheTtl5Min)
        conv2 <- store.getConversation sid
        conv2.cacheExpiresAt @?= expiry1,
      testCase "longer TTL extends cacheExpiresAt" $ do
        store <- newInMemoryConversationStore
        let sid :: Text = "test-session"
            msg = ClaudeMessage User [TextBlock "hello"]

        -- First append with 5m TTL
        store.appendMessages sid [msg] (Just CacheTtl5Min)
        conv1 <- store.getConversation sid

        -- Second append with 1h TTL — should extend
        store.appendMessages sid [msg] (Just CacheTtl1Hour)
        conv2 <- store.getConversation sid
        assertBool "1h TTL should extend past 5m TTL" $
          conv2.cacheExpiresAt > conv1.cacheExpiresAt,
      testCase "new conversation has epoch expiry (always expired)" $ do
        store <- newInMemoryConversationStore
        conv <- store.getConversation ("fresh" :: Text)
        conv.cacheExpiresAt @?= epoch,
      testCase "replaceMessages resets cacheExpiresAt to epoch" $ do
        store <- newInMemoryConversationStore
        let sid :: Text = "test-session"
            msg = ClaudeMessage User [TextBlock "hello"]

        -- First append to establish a non-epoch expiry
        store.appendMessages sid [msg] (Just CacheTtl1Hour)
        conv1 <- store.getConversation sid
        assertBool "expiry should be after epoch" $ conv1.cacheExpiresAt > epoch

        -- replaceMessages should reset to epoch
        store.replaceMessages sid [msg]
        conv2 <- store.getConversation sid
        conv2.cacheExpiresAt @?= epoch,
      testCase "appendMessages after replaceMessages gives now + ttl (not stale)" $ do
        store <- newInMemoryConversationStore
        let sid :: Text = "test-session"
            msg = ClaudeMessage User [TextBlock "hello"]

        -- Establish a large expiry with 1h TTL
        store.appendMessages sid [msg] (Just CacheTtl1Hour)
        conv1h <- store.getConversation sid
        let expiry1h = conv1h.cacheExpiresAt

        -- Reset via replaceMessages
        store.replaceMessages sid [msg]
        conv1 <- store.getConversation sid
        conv1.cacheExpiresAt @?= epoch

        -- appendMessages with 5m TTL should give now + 5m, not restore the old 1h expiry
        store.appendMessages sid [msg] (Just CacheTtl5Min)
        conv2 <- store.getConversation sid
        assertBool "expiry should be after epoch" $ conv2.cacheExpiresAt > epoch
        assertBool "5m TTL should not reach old 1h expiry" $
          conv2.cacheExpiresAt < expiry1h,
      testCase "extendCacheExpiry: old expiry wins when larger" $ do
        let now = UTCTime (fromGregorian 2024 6 1) 43200
            oldExpiry = addUTCTime 3600 now -- now + 1h
            -- 5m TTL: now + 300s < oldExpiry, so old wins
            result = extendCacheExpiry now CacheTtl5Min oldExpiry
        result @?= oldExpiry,
      testCase "extendCacheExpiry: new TTL wins when larger" $ do
        let now = UTCTime (fromGregorian 2024 6 1) 43200
            oldExpiry = addUTCTime 100 now -- old expires in 100s
            -- 1h TTL: now + 3600s > oldExpiry, so new wins
            result = extendCacheExpiry now CacheTtl1Hour oldExpiry
        result @?= addUTCTime 3600 now
    ]

-- | Minimal MessagesRequest for testing serialization
mkRequest :: Maybe ThinkingConfig -> Maybe OutputFormat -> MessagesRequest
mkRequest thk fmt =
  MessagesRequest
    { model = "claude-sonnet-4-20250514",
      maxTokens = 4096,
      system = Nothing,
      messages = [],
      tools = [],
      thinking = thk,
      cacheControl = Nothing,
      toolSearch = Nothing,
      outputFormat = fmt
    }

-- | Look up a key in the encoded JSON of a MessagesRequest
lookupField :: Aeson.Key -> MessagesRequest -> Maybe Value
lookupField key req = case Aeson.decode (encode req) of
  Just (Object obj) -> KM.lookup key obj
  _ -> Nothing

outputConfigTests :: TestTree
outputConfigTests =
  testGroup
    "output_config serialization"
    [ testCase "no thinking, no format -> no output_config" $ do
        let req = mkRequest Nothing Nothing
        lookupField "output_config" req @?= Nothing,
      testCase "adaptive thinking, no format -> effort only" $ do
        let req = mkRequest (Just (ThinkingConfigAdaptive (Just EffortHigh))) Nothing
        case lookupField "output_config" req of
          Just (Object oc) -> do
            KM.lookup "effort" oc @?= Just (Aeson.String "high")
            KM.member "format" oc @?= False
          other -> assertFailure $ "Expected output_config object, got: " <> show other,
      testCase "no thinking, with format -> format only" $ do
        let schema = Aeson.object ["type" Aeson..= ("object" :: String)]
            req = mkRequest Nothing (Just (jsonSchemaFormat schema))
        case lookupField "output_config" req of
          Just (Object oc) -> do
            KM.member "format" oc @?= True
            KM.member "effort" oc @?= False
          other -> assertFailure $ "Expected output_config object, got: " <> show other,
      testCase "adaptive thinking + format -> both" $ do
        let schema = Aeson.object ["type" Aeson..= ("object" :: String)]
            req = mkRequest (Just (ThinkingConfigAdaptive (Just EffortMedium))) (Just (jsonSchemaFormat schema))
        case lookupField "output_config" req of
          Just (Object oc) -> do
            KM.lookup "effort" oc @?= Just (Aeson.String "medium")
            KM.member "format" oc @?= True
          other -> assertFailure $ "Expected output_config object, got: " <> show other,
      testCase "adaptive thinking without effort -> no output_config" $ do
        let req = mkRequest (Just (ThinkingConfigAdaptive Nothing)) Nothing
        lookupField "output_config" req @?= Nothing,
      testCase "adaptive thinking without effort + format -> format only" $ do
        let schema = Aeson.object ["type" Aeson..= ("object" :: String)]
            req = mkRequest (Just (ThinkingConfigAdaptive Nothing)) (Just (jsonSchemaFormat schema))
        case lookupField "output_config" req of
          Just (Object oc) -> do
            KM.member "format" oc @?= True
            KM.member "effort" oc @?= False
          other -> assertFailure $ "Expected output_config object, got: " <> show other,
      testCase "budget thinking -> no output_config" $ do
        let req = mkRequest (Just (ThinkingConfigBudget 8000)) Nothing
        lookupField "output_config" req @?= Nothing
    ]
