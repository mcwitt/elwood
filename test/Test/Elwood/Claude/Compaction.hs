module Test.Elwood.Claude.Compaction (tests) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Elwood.Claude.Compaction (estimateTokens, extractText, formatMessagesForSummary, safeSplit)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolUseId (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Claude.Compaction"
    [ estimateTokensTests,
      extractTextTests,
      formatMessagesTests,
      safeSplitTests,
      estimateTokensProperties
    ]

estimateTokensTests :: TestTree
estimateTokensTests =
  testGroup
    "estimateTokens"
    [ testCase "empty list returns 0 or small number" $ do
        let tokens = estimateTokens []
        tokens < 10 @?= True, -- Empty JSON array is just "[]"
      testCase "single short message gives small count" $ do
        let msgs = [ClaudeMessage User [TextBlock "Hi"]]
            tokens = estimateTokens msgs
        tokens > 0 @?= True
        tokens < 100 @?= True,
      testCase "longer messages give higher counts" $ do
        let shortMsg = [ClaudeMessage User [TextBlock "Hi"]]
            longMsg = [ClaudeMessage User [TextBlock (T.replicate 1000 "word ")]]
        estimateTokens longMsg > estimateTokens shortMsg @?= True,
      testCase "multiple messages add up" $ do
        let oneMsg = [ClaudeMessage User [TextBlock "Hello"]]
            twoMsgs =
              [ ClaudeMessage User [TextBlock "Hello"],
                ClaudeMessage Assistant [TextBlock "Hi there"]
              ]
        estimateTokens twoMsgs > estimateTokens oneMsg @?= True
    ]

extractTextTests :: TestTree
extractTextTests =
  testGroup
    "extractText"
    [ testCase "extracts text from TextBlock" $ do
        let blocks = [TextBlock "Hello, world!"]
        extractText blocks @?= "Hello, world!",
      testCase "concatenates multiple TextBlocks" $ do
        let blocks = [TextBlock "Hello", TextBlock "World"]
        extractText blocks @?= "Hello\nWorld",
      testCase "ignores ToolUseBlock" $ do
        let blocks =
              [ TextBlock "Before",
                ToolUseBlock (ToolUseId "id") "tool" (Aeson.object []),
                TextBlock "After"
              ]
        extractText blocks @?= "Before\nAfter",
      testCase "ignores ToolResultBlock" $ do
        let blocks =
              [ TextBlock "Start",
                ToolResultBlock (ToolUseId "id") "result" False,
                TextBlock "End"
              ]
        extractText blocks @?= "Start\nEnd",
      testCase "empty list returns empty string" $
        extractText [] @?= ""
    ]

formatMessagesTests :: TestTree
formatMessagesTests =
  testGroup
    "formatMessagesForSummary"
    [ testCase "formats user message" $ do
        let msgs = [ClaudeMessage User [TextBlock "Hello"]]
            formatted = formatMessagesForSummary msgs
        T.isInfixOf "User:" formatted @?= True
        T.isInfixOf "Hello" formatted @?= True,
      testCase "formats assistant message" $ do
        let msgs = [ClaudeMessage Assistant [TextBlock "Hi there"]]
            formatted = formatMessagesForSummary msgs
        T.isInfixOf "Assistant:" formatted @?= True
        T.isInfixOf "Hi there" formatted @?= True,
      testCase "formats conversation" $ do
        let msgs =
              [ ClaudeMessage User [TextBlock "What's the weather?"],
                ClaudeMessage Assistant [TextBlock "It's sunny today."]
              ]
            formatted = formatMessagesForSummary msgs
        T.isInfixOf "User:" formatted @?= True
        T.isInfixOf "Assistant:" formatted @?= True
        T.isInfixOf "weather" formatted @?= True
        T.isInfixOf "sunny" formatted @?= True,
      testCase "truncates very long messages" $ do
        let longText = T.replicate 2000 "x"
            msgs = [ClaudeMessage User [TextBlock longText]]
            formatted = formatMessagesForSummary msgs
        -- Should be truncated to ~1000 chars plus "..."
        T.length formatted < 1100 @?= True
        T.isSuffixOf "..." formatted @?= True
    ]

safeSplitTests :: TestTree
safeSplitTests =
  testGroup
    "safeSplit"
    [ testCase "splits normally when no tool_result at boundary" $ do
        let msgs =
              [ ClaudeMessage User [TextBlock "Hello"],
                ClaudeMessage Assistant [TextBlock "Hi there"],
                ClaudeMessage User [TextBlock "How are you?"],
                ClaudeMessage Assistant [TextBlock "I'm good"]
              ]
            (old, recent) = safeSplit 2 msgs
        length old @?= 2
        length recent @?= 2,
      testCase "adjusts split when tool_result at boundary" $ do
        let msgs =
              [ ClaudeMessage User [TextBlock "Hello"],
                ClaudeMessage Assistant [ToolUseBlock (ToolUseId "tool-1") "some_tool" (Aeson.object [])],
                ClaudeMessage User [ToolResultBlock (ToolUseId "tool-1") "result" False],
                ClaudeMessage Assistant [TextBlock "Done"]
              ]
            -- Naive split at 2 would put tool_result in recent without its tool_use
            (old, recent) = safeSplit 2 msgs
        -- Should adjust to include the tool_use message in recent
        length old @?= 1
        length recent @?= 3,
      testCase "handles multiple tool_use/tool_result pairs" $ do
        let msgs =
              [ ClaudeMessage User [TextBlock "Do two things"],
                ClaudeMessage Assistant [ToolUseBlock (ToolUseId "tool-1") "first_tool" (Aeson.object [])],
                ClaudeMessage User [ToolResultBlock (ToolUseId "tool-1") "result1" False],
                ClaudeMessage Assistant [ToolUseBlock (ToolUseId "tool-2") "second_tool" (Aeson.object [])],
                ClaudeMessage User [ToolResultBlock (ToolUseId "tool-2") "result2" False],
                ClaudeMessage Assistant [TextBlock "Both done"]
              ]
            -- Split at 4 would put tool_result at start of recent
            (old, recent) = safeSplit 4 msgs
        -- Should adjust to include the tool_use
        length old @?= 3
        length recent @?= 3,
      testCase "handles empty list" $ do
        let (old, recent) = safeSplit 0 ([] :: [ClaudeMessage])
        old @?= []
        recent @?= [],
      testCase "handles split at 0" $ do
        let msgs = [ClaudeMessage User [TextBlock "Hello"]]
            (old, recent) = safeSplit 0 msgs
        old @?= []
        length recent @?= 1,
      testCase "handles split beyond length" $ do
        let msgs = [ClaudeMessage User [TextBlock "Hello"]]
            (old, recent) = safeSplit 10 msgs
        length old @?= 1
        recent @?= [],
      testCase "no adjustment needed when recent starts with text" $ do
        let msgs =
              [ ClaudeMessage Assistant [ToolUseBlock (ToolUseId "tool-1") "some_tool" (Aeson.object [])],
                ClaudeMessage User [ToolResultBlock (ToolUseId "tool-1") "result" False],
                ClaudeMessage User [TextBlock "Thanks"],
                ClaudeMessage Assistant [TextBlock "You're welcome"]
              ]
            -- Split at 2 puts text message at start of recent
            (old, recent) = safeSplit 2 msgs
        length old @?= 2
        length recent @?= 2
    ]

estimateTokensProperties :: TestTree
estimateTokensProperties =
  testGroup
    "estimateTokens properties"
    [ testProperty "always non-negative" $
        \(msgs :: [SimpleMessage]) ->
          estimateTokens (map toClaudeMessage msgs) >= 0,
      testProperty "more content means more tokens" $
        \(base :: String) ->
          let short = [ClaudeMessage User [TextBlock (T.pack base)]]
              long = [ClaudeMessage User [TextBlock (T.pack (base ++ base ++ base))]]
           in null base || estimateTokens long >= estimateTokens short
    ]

-- | Simple message type for QuickCheck
data SimpleMessage = SimpleMessage Role String
  deriving (Show)

instance Arbitrary SimpleMessage where
  arbitrary = SimpleMessage <$> elements [User, Assistant] <*> arbitrary

toClaudeMessage :: SimpleMessage -> ClaudeMessage
toClaudeMessage (SimpleMessage role content) =
  ClaudeMessage role [TextBlock (T.pack content)]
