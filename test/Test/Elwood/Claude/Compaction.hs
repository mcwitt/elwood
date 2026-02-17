module Test.Elwood.Claude.Compaction (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..))
import Elwood.Claude.Compaction (estimateTokens, extractText, formatMessagesForSummary)

tests :: TestTree
tests = testGroup "Claude.Compaction"
  [ estimateTokensTests
  , extractTextTests
  , formatMessagesTests
  , estimateTokensProperties
  ]

estimateTokensTests :: TestTree
estimateTokensTests = testGroup "estimateTokens"
  [ testCase "empty list returns 0 or small number" $ do
      let tokens = estimateTokens []
      tokens < 10 @?= True  -- Empty JSON array is just "[]"

  , testCase "single short message gives small count" $ do
      let msgs = [ClaudeMessage User [TextBlock "Hi"]]
          tokens = estimateTokens msgs
      tokens > 0 @?= True
      tokens < 100 @?= True

  , testCase "longer messages give higher counts" $ do
      let shortMsg = [ClaudeMessage User [TextBlock "Hi"]]
          longMsg = [ClaudeMessage User [TextBlock (T.replicate 1000 "word ")]]
      estimateTokens longMsg > estimateTokens shortMsg @?= True

  , testCase "multiple messages add up" $ do
      let oneMsg = [ClaudeMessage User [TextBlock "Hello"]]
          twoMsgs =
            [ ClaudeMessage User [TextBlock "Hello"]
            , ClaudeMessage Assistant [TextBlock "Hi there"]
            ]
      estimateTokens twoMsgs > estimateTokens oneMsg @?= True
  ]

extractTextTests :: TestTree
extractTextTests = testGroup "extractText"
  [ testCase "extracts text from TextBlock" $ do
      let blocks = [TextBlock "Hello, world!"]
      extractText blocks @?= "Hello, world!"

  , testCase "concatenates multiple TextBlocks" $ do
      let blocks = [TextBlock "Hello", TextBlock "World"]
      extractText blocks @?= "Hello\nWorld"

  , testCase "ignores ToolUseBlock" $ do
      let blocks =
            [ TextBlock "Before"
            , ToolUseBlock "id" "tool" (Aeson.object [])
            , TextBlock "After"
            ]
      extractText blocks @?= "Before\nAfter"

  , testCase "ignores ToolResultBlock" $ do
      let blocks =
            [ TextBlock "Start"
            , ToolResultBlock "id" "result" False
            , TextBlock "End"
            ]
      extractText blocks @?= "Start\nEnd"

  , testCase "empty list returns empty string" $
      extractText [] @?= ""
  ]

formatMessagesTests :: TestTree
formatMessagesTests = testGroup "formatMessagesForSummary"
  [ testCase "formats user message" $ do
      let msgs = [ClaudeMessage User [TextBlock "Hello"]]
          formatted = formatMessagesForSummary msgs
      T.isInfixOf "User:" formatted @?= True
      T.isInfixOf "Hello" formatted @?= True

  , testCase "formats assistant message" $ do
      let msgs = [ClaudeMessage Assistant [TextBlock "Hi there"]]
          formatted = formatMessagesForSummary msgs
      T.isInfixOf "Assistant:" formatted @?= True
      T.isInfixOf "Hi there" formatted @?= True

  , testCase "formats conversation" $ do
      let msgs =
            [ ClaudeMessage User [TextBlock "What's the weather?"]
            , ClaudeMessage Assistant [TextBlock "It's sunny today."]
            ]
          formatted = formatMessagesForSummary msgs
      T.isInfixOf "User:" formatted @?= True
      T.isInfixOf "Assistant:" formatted @?= True
      T.isInfixOf "weather" formatted @?= True
      T.isInfixOf "sunny" formatted @?= True

  , testCase "truncates very long messages" $ do
      let longText = T.replicate 2000 "x"
          msgs = [ClaudeMessage User [TextBlock longText]]
          formatted = formatMessagesForSummary msgs
      -- Should be truncated to ~1000 chars plus "..."
      T.length formatted < 1100 @?= True
      T.isSuffixOf "..." formatted @?= True
  ]

estimateTokensProperties :: TestTree
estimateTokensProperties = testGroup "estimateTokens properties"
  [ testProperty "always non-negative" $
      \(msgs :: [SimpleMessage]) ->
        estimateTokens (map toClaudeMessage msgs) >= 0

  , testProperty "more content means more tokens" $
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
