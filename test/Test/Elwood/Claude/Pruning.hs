module Test.Elwood.Claude.Pruning (tests) where

import Data.Aeson qualified as Aeson
import Elwood.Claude.Pruning (getAndUpdateHorizon, newPruneHorizons, pruneToolResults, prunedPlaceholder)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.Pruning"
    [ pruneBeforeHorizon,
      preserveAtAndAfterHorizon,
      preserveErrors,
      preserveNonToolResultBlocks,
      neverModifyAssistantMessages,
      horizonBeyondLength,
      horizonZero,
      emptyMessages,
      idempotent,
      preservesCountAndOrder,
      horizonClampedAfterCompaction
    ]

-- | Tool results before the horizon are replaced with placeholders
pruneBeforeHorizon :: TestTree
pruneBeforeHorizon =
  testCase "prunes tool results before horizon" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "big output" False],
            ClaudeMessage User [ToolResultBlock "t2" "more output" False],
            ClaudeMessage User [TextBlock "question"]
          ]
        result = pruneToolResults 2 msgs
    case result of
      [m1, m2, m3] -> do
        m1.content @?= [ToolResultBlock "t1" prunedPlaceholder False]
        m2.content @?= [ToolResultBlock "t2" prunedPlaceholder False]
        m3.content @?= [TextBlock "question"]
      _ -> assertFailure "Expected 3 messages"

-- | Messages at and after the horizon are untouched
preserveAtAndAfterHorizon :: TestTree
preserveAtAndAfterHorizon =
  testCase "preserves messages at/after horizon" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "old" False],
            ClaudeMessage User [ToolResultBlock "t2" "recent" False],
            ClaudeMessage User [ToolResultBlock "t3" "newest" False]
          ]
        result = pruneToolResults 1 msgs
    case result of
      [m1, m2, m3] -> do
        m1.content @?= [ToolResultBlock "t1" prunedPlaceholder False]
        m2.content @?= [ToolResultBlock "t2" "recent" False]
        m3.content @?= [ToolResultBlock "t3" "newest" False]
      _ -> assertFailure "Expected 3 messages"

-- | Error results (isError = True) are never pruned
preserveErrors :: TestTree
preserveErrors =
  testCase "preserves error results" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "error msg" True],
            ClaudeMessage User [ToolResultBlock "t2" "success" False]
          ]
        result = pruneToolResults 2 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [ToolResultBlock "t1" "error msg" True]
        m2.content @?= [ToolResultBlock "t2" prunedPlaceholder False]
      _ -> assertFailure "Expected 2 messages"

-- | Non-ToolResultBlock content is preserved (TextBlocks, ThinkingBlocks)
preserveNonToolResultBlocks :: TestTree
preserveNonToolResultBlocks =
  testCase "preserves non-ToolResultBlock content" $ do
    let msgs =
          [ ClaudeMessage User [TextBlock "hello", ToolResultBlock "t1" "big" False],
            ClaudeMessage User [TextBlock "world"]
          ]
        result = pruneToolResults 2 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [TextBlock "hello", ToolResultBlock "t1" prunedPlaceholder False]
        m2.content @?= [TextBlock "world"]
      _ -> assertFailure "Expected 2 messages"

-- | Assistant messages are never modified
neverModifyAssistantMessages :: TestTree
neverModifyAssistantMessages =
  testCase "never modifies assistant messages" $ do
    let toolUse = ToolUseBlock "t1" "some_tool" (Aeson.object [])
        msgs =
          [ ClaudeMessage Assistant [toolUse],
            ClaudeMessage User [ToolResultBlock "t1" "result" False]
          ]
        result = pruneToolResults 2 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [toolUse]
        m2.content @?= [ToolResultBlock "t1" prunedPlaceholder False]
      _ -> assertFailure "Expected 2 messages"

-- | Horizon beyond list length prunes all eligible results (clamp)
horizonBeyondLength :: TestTree
horizonBeyondLength =
  testCase "horizon beyond list length prunes all" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "data" False],
            ClaudeMessage User [ToolResultBlock "t2" "data" False]
          ]
        result = pruneToolResults 100 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [ToolResultBlock "t1" prunedPlaceholder False]
        m2.content @?= [ToolResultBlock "t2" prunedPlaceholder False]
      _ -> assertFailure "Expected 2 messages"

-- | Horizon = 0 is a no-op
horizonZero :: TestTree
horizonZero =
  testCase "horizon 0 is no-op" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "data" False]
          ]
        result = pruneToolResults 0 msgs
    result @?= msgs

-- | Empty message list returns empty
emptyMessages :: TestTree
emptyMessages =
  testCase "empty message list returns empty" $
    pruneToolResults 5 [] @?= []

-- | Idempotent: prune n (prune n msgs) == prune n msgs
idempotent :: TestTree
idempotent =
  testCase "idempotent" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock "t1" "data" False],
            ClaudeMessage Assistant [TextBlock "reply"],
            ClaudeMessage User [ToolResultBlock "t2" "more" False]
          ]
        once = pruneToolResults 2 msgs
        twice = pruneToolResults 2 once
    twice @?= once

-- | Message count and role order are preserved
preservesCountAndOrder :: TestTree
preservesCountAndOrder =
  testCase "preserves message count and role order" $ do
    let msgs =
          [ ClaudeMessage User [TextBlock "q1"],
            ClaudeMessage Assistant [TextBlock "a1"],
            ClaudeMessage User [ToolResultBlock "t1" "big" False],
            ClaudeMessage Assistant [TextBlock "a2"]
          ]
        result = pruneToolResults 3 msgs
    length result @?= length msgs
    map (.role) result @?= map (.role) msgs

-- | Stale horizon from before compaction is clamped to actual message count
horizonClampedAfterCompaction :: TestTree
horizonClampedAfterCompaction =
  testCase "getAndUpdateHorizon clamps stale horizon after compaction" $ do
    horizons <- newPruneHorizons
    -- Simulate: cache expires with 100 messages, storing horizon = 100
    h1 <- getAndUpdateHorizon horizons "sess1" 100 True
    h1 @?= 100
    -- Simulate: compaction shrinks history to 50, cache not expired
    h2 <- getAndUpdateHorizon horizons "sess1" 50 False
    h2 @?= 50 -- must be clamped, not 100
