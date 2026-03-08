module Test.Elwood.Claude.Compaction (tests) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Elwood.Claude.Compaction (estimateTokens, extractText, formatMessagesForSummary, strategySplit)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolUseId (..), turnBoundaryIndices)
import Elwood.Config (CompactionStrategy (..))
import Elwood.Positive (unsafePositive)
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
      strategySplitTests,
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

strategySplitTests :: TestTree
strategySplitTests =
  testGroup
    "strategySplit"
    [ testGroup
        "CKeepTurns"
        [ testCase "keeps last N turns, compacts the rest" $ do
            -- 3 turns: user/assistant pairs with text boundaries
            let msgs =
                  [ ClaudeMessage User [TextBlock "Turn 1"],
                    ClaudeMessage Assistant [TextBlock "Response 1"],
                    ClaudeMessage User [TextBlock "Turn 2"],
                    ClaudeMessage Assistant [TextBlock "Response 2"],
                    ClaudeMessage User [TextBlock "Turn 3"],
                    ClaudeMessage Assistant [TextBlock "Response 3"]
                  ]
            case strategySplit (CKeepTurns (unsafePositive 2)) (unsafePositive 50000) msgs of
              Nothing -> assertFailure "Expected Just, got Nothing"
              Just (old, recent) -> do
                length old @?= 2 -- Turn 1 + Response 1
                length recent @?= 4,
          testCase "returns Nothing when <= N turns exist" $ do
            let msgs =
                  [ ClaudeMessage User [TextBlock "Turn 1"],
                    ClaudeMessage Assistant [TextBlock "Response 1"],
                    ClaudeMessage User [TextBlock "Turn 2"],
                    ClaudeMessage Assistant [TextBlock "Response 2"]
                  ]
            strategySplit (CKeepTurns (unsafePositive 2)) (unsafePositive 50000) msgs @?= Nothing
            strategySplit (CKeepTurns (unsafePositive 3)) (unsafePositive 50000) msgs @?= Nothing,
          testCase "splits at turn boundary with tool pairs before it" $ do
            -- Turn boundaries at 0, 3, 5. CKeepTurns 2 splits at index 3.
            -- Tool pairs before the boundary stay in old.
            let msgs =
                  [ ClaudeMessage User [TextBlock "Turn 1"],
                    ClaudeMessage Assistant [ToolUseBlock (ToolUseId "t1") "tool" (Aeson.object [])],
                    ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "result" False],
                    ClaudeMessage User [TextBlock "Turn 2"],
                    ClaudeMessage Assistant [TextBlock "Response 2"],
                    ClaudeMessage User [TextBlock "Turn 3"],
                    ClaudeMessage Assistant [TextBlock "Response 3"]
                  ]
            case strategySplit (CKeepTurns (unsafePositive 2)) (unsafePositive 50000) msgs of
              Nothing -> assertFailure "Expected Just, got Nothing"
              Just (old, recent) -> do
                length old @?= 3 -- Turn 1 + tool_use + tool_result
                length recent @?= 4,
          testCase "empty message list returns Nothing" $
            strategySplit (CKeepTurns (unsafePositive 5)) (unsafePositive 50000) [] @?= Nothing
        ],
      testGroup
        "CKeepFraction"
        [ testCase "small fraction compacts most messages" $ do
            -- Make turn 1 very large so it dominates the token count.
            -- Use a very large threshold so the fraction math gives a
            -- keep budget smaller than the big message.
            let bigText = T.replicate 2000 "word "
                msgs =
                  [ ClaudeMessage User [TextBlock "Turn 1"],
                    ClaudeMessage Assistant [TextBlock bigText],
                    ClaudeMessage User [TextBlock "Turn 2"],
                    ClaudeMessage Assistant [TextBlock "Short"],
                    ClaudeMessage User [TextBlock "Turn 3"],
                    ClaudeMessage Assistant [TextBlock "Short"]
                  ]
                -- Use a threshold large enough that 0.01 * threshold
                -- still exceeds the small messages but not the big one
                threshold = unsafePositive (estimateTokens msgs * 10)
            case strategySplit (CKeepFraction 0.01) threshold msgs of
              Nothing -> assertFailure "Expected Just, got Nothing"
              Just (old, recent) -> do
                -- Turn boundaries: 0, 2, 4. The big message is in turn 1.
                assertBool "old should be non-empty" (not (null old))
                assertBool "recent should be non-empty" (not (null recent))
                -- The split should land on a turn boundary
                let recentBoundaries = turnBoundaryIndices recent
                assertBool "recent starts at a turn boundary" (not (null recentBoundaries)),
          testCase "large fraction keeps most messages" $ do
            let msgs =
                  [ ClaudeMessage User [TextBlock "Turn 1"],
                    ClaudeMessage Assistant [TextBlock "Response 1"],
                    ClaudeMessage User [TextBlock "Turn 2"],
                    ClaudeMessage Assistant [TextBlock "Response 2"],
                    ClaudeMessage User [TextBlock "Turn 3"],
                    ClaudeMessage Assistant [TextBlock "Response 3"]
                  ]
                totalTokens = unsafePositive (estimateTokens msgs)
            -- fraction=1.0 means keep all tokens → no-op
            strategySplit (CKeepFraction 1.0) totalTokens msgs @?= Nothing,
          testCase "returns Nothing when all messages fit in keep region" $ do
            let msgs =
                  [ ClaudeMessage User [TextBlock "Hello"],
                    ClaudeMessage Assistant [TextBlock "Hi"]
                  ]
            -- threshold much larger than actual tokens
            strategySplit (CKeepFraction 1.0) (unsafePositive 100000) msgs @?= Nothing,
          testCase "empty message list returns Nothing" $
            strategySplit (CKeepFraction 0.25) (unsafePositive 50000) [] @?= Nothing
        ],
      testGroup
        "turnBoundaryIndices"
        [ testCase "identifies user messages with TextBlock" $ do
            let msgs =
                  [ ClaudeMessage User [TextBlock "Hello"],
                    ClaudeMessage Assistant [TextBlock "Hi"],
                    ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "res" False],
                    ClaudeMessage User [TextBlock "Question"],
                    ClaudeMessage Assistant [TextBlock "Answer"]
                  ]
            turnBoundaryIndices msgs @?= [0, 3],
          testCase "empty list" $
            turnBoundaryIndices [] @?= []
        ]
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
