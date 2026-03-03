module Test.Elwood.Claude.Pruning (tests) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Elwood.Claude.Pruning (getAndUpdateHorizon, newPruneHorizons, protectedBoundary, pruneToolResults, softPrune)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolUseId (..))
import Elwood.Config (PruningConfig (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | Small config for easy test verification
testConfig :: PruningConfig
testConfig = PruningConfig {headChars = 10, tailChars = 10, keepTurns = 3}

tests :: TestTree
tests =
  testGroup
    "Claude.Pruning"
    [ -- softPrune unit tests
      softPruneShortContent,
      softPruneLongContent,
      softPruneIdempotent,
      -- protectedBoundary tests
      protectedBoundaryBasic,
      protectedBoundaryFewerTurns,
      protectedBoundaryEmpty,
      -- pruneToolResults tests (updated from original)
      pruneBeforeHorizon,
      preserveAtAndAfterHorizon,
      preserveErrors,
      preserveNonToolResultBlocks,
      neverModifyAssistantMessages,
      horizonBeyondLength,
      horizonZero,
      emptyMessages,
      idempotent,
      preservesCountAndOrder,
      horizonClampedAfterCompaction,
      -- recency protection tests
      recencyProtectsRecentTurns,
      recencyClipsHorizon,
      shortResultsUnchanged
    ]

-- ---------------------------------------------------------------------------
-- softPrune tests
-- ---------------------------------------------------------------------------

-- | Short content is returned unchanged
softPruneShortContent :: TestTree
softPruneShortContent =
  testCase "softPrune: short content unchanged" $ do
    let content = "hello world"
    softPrune 10 10 content @?= content

-- | Long content is pruned with head/tail and indicator
softPruneLongContent :: TestTree
softPruneLongContent =
  testCase "softPrune: long content pruned with indicator" $ do
    let content = T.replicate 200 "x"
        result = softPrune 10 10 content
    -- Should start with 10 x's
    T.take 10 result @?= T.replicate 10 "x"
    -- Should end with 10 x's
    T.takeEnd 10 result @?= T.replicate 10 "x"
    -- Should contain the pruning indicator
    assertBool "contains pruning indicator" (T.isInfixOf "pruned 180 of 200 characters" result)
    -- Should be shorter than original
    assertBool "result shorter than original" (T.length result < T.length content)

-- | softPrune is idempotent
softPruneIdempotent :: TestTree
softPruneIdempotent =
  testCase "softPrune: idempotent" $ do
    let content = T.replicate 200 "x"
        once = softPrune 10 10 content
        twice = softPrune 10 10 once
    twice @?= once

-- ---------------------------------------------------------------------------
-- protectedBoundary tests
-- ---------------------------------------------------------------------------

-- | Correctly identifies turn boundaries and protects last N turns
protectedBoundaryBasic :: TestTree
protectedBoundaryBasic =
  testCase "protectedBoundary: protects last N turns" $ do
    let msgs =
          [ ClaudeMessage User [TextBlock "turn 1"], -- index 0 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 1"], -- index 1
            ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "result" False], -- index 2 (iteration, not turn)
            ClaudeMessage Assistant [TextBlock "reply 2"], -- index 3
            ClaudeMessage User [TextBlock "turn 2"], -- index 4 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 3"], -- index 5
            ClaudeMessage User [TextBlock "turn 3"], -- index 6 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 4"] -- index 7
          ]
    -- keepTurns = 2: protect last 2 turns → boundary at index 4
    protectedBoundary 2 msgs @?= 4
    -- keepTurns = 3: protect last 3 turns → boundary at index 0
    protectedBoundary 3 msgs @?= 0
    -- keepTurns = 1: protect last 1 turn → boundary at index 6
    protectedBoundary 1 msgs @?= 6

-- | Fewer turns than keepTurns → protect everything
protectedBoundaryFewerTurns :: TestTree
protectedBoundaryFewerTurns =
  testCase "protectedBoundary: fewer turns than keepTurns protects all" $ do
    let msgs =
          [ ClaudeMessage User [TextBlock "only turn"],
            ClaudeMessage Assistant [TextBlock "reply"]
          ]
    -- keepTurns = 5 but only 1 turn exists → boundary at 0 (protect everything)
    protectedBoundary 5 msgs @?= 0

-- | Empty messages
protectedBoundaryEmpty :: TestTree
protectedBoundaryEmpty =
  testCase "protectedBoundary: empty messages" $
    protectedBoundary 3 [] @?= 0

-- ---------------------------------------------------------------------------
-- pruneToolResults tests (updated for PruningConfig)
-- ---------------------------------------------------------------------------

-- | Tool results before the horizon are soft-pruned
pruneBeforeHorizon :: TestTree
pruneBeforeHorizon =
  testCase "soft-prunes tool results before horizon" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False],
            ClaudeMessage User [TextBlock "question"]
          ]
        -- keepTurns = 0 so recency doesn't interfere
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 2 msgs
    case result of
      [m1, m2, m3] -> do
        -- Soft-pruned: should contain indicator, not be the full original
        case m1.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m1 contains pruning indicator" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m1"
        case m2.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m2 contains pruning indicator" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m2"
        m3.content @?= [TextBlock "question"]
      _ -> assertFailure "Expected 3 messages"

-- | Messages at and after the horizon are untouched
preserveAtAndAfterHorizon :: TestTree
preserveAtAndAfterHorizon =
  testCase "preserves messages at/after horizon" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t3") bigOutput False]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 1 msgs
    case result of
      [m1, m2, m3] -> do
        case m1.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m1 pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m1"
        -- m2 and m3 should be unchanged
        m2.content @?= [ToolResultBlock (ToolUseId "t2") bigOutput False]
        m3.content @?= [ToolResultBlock (ToolUseId "t3") bigOutput False]
      _ -> assertFailure "Expected 3 messages"

-- | Error results (isError = True) are never pruned
preserveErrors :: TestTree
preserveErrors =
  testCase "preserves error results" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "error msg" True],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") (T.replicate 200 "x") False]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 2 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [ToolResultBlock (ToolUseId "t1") "error msg" True]
        case m2.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m2 pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m2"
      _ -> assertFailure "Expected 2 messages"

-- | Non-ToolResultBlock content is preserved (TextBlocks, ThinkingBlocks)
preserveNonToolResultBlocks :: TestTree
preserveNonToolResultBlocks =
  testCase "preserves non-ToolResultBlock content" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [TextBlock "hello", ToolResultBlock (ToolUseId "t1") bigOutput False],
            ClaudeMessage User [TextBlock "world"]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 2 msgs
    case result of
      [m1, m2] -> do
        case m1.content of
          [TextBlock "hello", ToolResultBlock _ txt False] ->
            assertBool "tool result pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected TextBlock + ToolResultBlock in m1"
        m2.content @?= [TextBlock "world"]
      _ -> assertFailure "Expected 2 messages"

-- | Assistant messages are never modified
neverModifyAssistantMessages :: TestTree
neverModifyAssistantMessages =
  testCase "never modifies assistant messages" $ do
    let toolUse = ToolUseBlock (ToolUseId "t1") "some_tool" (Aeson.object [])
        bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage Assistant [toolUse],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 2 msgs
    case result of
      [m1, m2] -> do
        m1.content @?= [toolUse]
        case m2.content of
          [ToolResultBlock _ txt False] ->
            assertBool "tool result pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m2"
      _ -> assertFailure "Expected 2 messages"

-- | Horizon beyond list length prunes all eligible results (clamp)
horizonBeyondLength :: TestTree
horizonBeyondLength =
  testCase "horizon beyond list length prunes all" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 100 msgs
    case result of
      [m1, m2] -> do
        case m1.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m1 pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m1"
        case m2.content of
          [ToolResultBlock _ txt False] ->
            assertBool "m2 pruned" (T.isInfixOf "pruned" txt)
          _ -> assertFailure "Expected single ToolResultBlock in m2"
      _ -> assertFailure "Expected 2 messages"

-- | Horizon = 0 is a no-op
horizonZero :: TestTree
horizonZero =
  testCase "horizon 0 is no-op" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "data" False]
          ]
        result = pruneToolResults testConfig 0 msgs
    result @?= msgs

-- | Empty message list returns empty
emptyMessages :: TestTree
emptyMessages =
  testCase "empty message list returns empty" $
    pruneToolResults testConfig 5 [] @?= []

-- | Idempotent: prune n (prune n msgs) == prune n msgs
idempotent :: TestTree
idempotent =
  testCase "idempotent" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False],
            ClaudeMessage Assistant [TextBlock "reply"],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False]
          ]
        cfg = testConfig {keepTurns = 0}
        once = pruneToolResults cfg 2 msgs
        twice = pruneToolResults cfg 2 once
    twice @?= once

-- | Message count and role order are preserved
preservesCountAndOrder :: TestTree
preservesCountAndOrder =
  testCase "preserves message count and role order" $ do
    let msgs =
          [ ClaudeMessage User [TextBlock "q1"],
            ClaudeMessage Assistant [TextBlock "a1"],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t1") (T.replicate 200 "x") False],
            ClaudeMessage Assistant [TextBlock "a2"]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 3 msgs
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

-- ---------------------------------------------------------------------------
-- Recency protection tests
-- ---------------------------------------------------------------------------

-- | Tool results in last 3 turns are untouched even before horizon
recencyProtectsRecentTurns :: TestTree
recencyProtectsRecentTurns =
  testCase "recency protection: last keepTurns turns untouched" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [TextBlock "turn 1"], -- turn boundary
            ClaudeMessage Assistant [TextBlock "reply 1"],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False], -- iteration
            ClaudeMessage User [TextBlock "turn 2"], -- turn boundary
            ClaudeMessage Assistant [TextBlock "reply 2"],
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False], -- iteration
            ClaudeMessage User [TextBlock "turn 3"], -- turn boundary
            ClaudeMessage Assistant [TextBlock "reply 3"]
          ]
        -- horizon = 100 (would prune everything), but keepTurns = 3
        cfg = testConfig {keepTurns = 3}
        result = pruneToolResults cfg 100 msgs
    -- All 3 turns should be protected, so nothing is pruned
    result @?= msgs

-- | Horizon within protected region is clipped to the boundary
recencyClipsHorizon :: TestTree
recencyClipsHorizon =
  testCase "recency protection clips horizon to boundary" $ do
    let bigOutput = T.replicate 200 "x"
        msgs =
          [ ClaudeMessage User [TextBlock "turn 1"], -- index 0 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 1"], -- index 1
            ClaudeMessage User [ToolResultBlock (ToolUseId "t1") bigOutput False], -- index 2
            ClaudeMessage User [TextBlock "turn 2"], -- index 3 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 2"], -- index 4
            ClaudeMessage User [ToolResultBlock (ToolUseId "t2") bigOutput False], -- index 5
            ClaudeMessage User [TextBlock "turn 3"], -- index 6 (turn boundary)
            ClaudeMessage Assistant [TextBlock "reply 3"] -- index 7
          ]
        -- horizon = 5 would prune indices 0-4, but keepTurns = 2
        -- protects from index 3 onward, so effective horizon = 3
        cfg = testConfig {keepTurns = 2}
        result = pruneToolResults cfg 5 msgs
    -- Index 2 (tool result before boundary) should be pruned
    case result !! 2 of
      ClaudeMessage _ [ToolResultBlock _ txt False] ->
        assertBool "pre-boundary tool result pruned" (T.isInfixOf "pruned" txt)
      _ -> assertFailure "Expected pruned ToolResultBlock at index 2"
    -- Index 5 (tool result within protected turns) should be untouched
    (result !! 5).content @?= [ToolResultBlock (ToolUseId "t2") bigOutput False]

-- | Short results are left unchanged even before horizon (softPrune no-op)
shortResultsUnchanged :: TestTree
shortResultsUnchanged =
  testCase "short results unchanged even before horizon" $ do
    let msgs =
          [ ClaudeMessage User [ToolResultBlock (ToolUseId "t1") "small" False],
            ClaudeMessage User [TextBlock "question"]
          ]
        cfg = testConfig {keepTurns = 0}
        result = pruneToolResults cfg 2 msgs
    -- "small" is shorter than headChars + tailChars + overhead, so unchanged
    result @?= msgs
