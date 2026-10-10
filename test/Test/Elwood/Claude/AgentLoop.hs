module Test.Elwood.Claude.AgentLoop (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, finally, try)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.AgentSettings (AgentPreset (..), AgentProfile (..), ModelRef (..), ToolFilter (..), ToolSearchConfig (..))
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), ExhaustionInfo (..), perceiveResultImages, runAgentTurn)
import Elwood.Claude.Client (ClaudeClient (..))
import Elwood.Claude.Compaction (compactMessages)
import Elwood.Claude.Observer (AgentObserver (..), ToolUseCallback)
import Elwood.Claude.Turn qualified as Turn
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), StopReason (..), ToolName (..), ToolResultPart (..), ToolSchema (..), ToolUseId (..))
import Elwood.Config (CompactionConfig (..), CompactionStrategy (..))
import Elwood.Metrics (newMetricsStore)
import Elwood.Permissions (resolvePermissions)
import Elwood.Positive (literal)
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Thinking (ThinkingDisplay (..), ThinkingEffort (..), ThinkingMode (..))
import Elwood.Tools.AsyncTask qualified as Tasks
import Elwood.Tools.Delegate (mkDelegateTaskTool)
import Elwood.Tools.Registry (ToolRegistry, newToolRegistry, registerTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..), noApprovalChannel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status200, status429)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket, setPort)
import System.Timeout (timeout)
import Test.Elwood.TestImage (mkPngBytes)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.AgentLoop"
    [ cancellationTests,
      decisionTests,
      intermediateDeliveryTests,
      protocolTests,
      turnExecutionTests,
      perceiveResultImagesTests
    ]

cancellationTests :: TestTree
cancellationTests =
  testGroup
    "cancellation"
    [ testCase "immediately cancelled turn returns AgentCancelled" $ do
        cfg <- mkTestConfig (pure True)
        let userMsg = ClaudeMessage User [TextBlock "hello"]
        result <- runAgentTurn cfg [] userMsg
        case result of
          AgentCancelled -> pure ()
          other -> assertFailure $ "expected AgentCancelled, got: " ++ show other,
      testCase "non-cancelled turn attempts the API call" $ do
        cfg <- mkTestConfig (pure False)
        -- The dummy client will throw an HttpException (connection refused).
        -- If we get an exception, the API was attempted — not cancelled.
        let userMsg = ClaudeMessage User [TextBlock "hello"]
        result <- try @SomeException $ runAgentTurn cfg [] userMsg
        case result of
          Left _ -> pure () -- expected: dummy client connection refused
          Right AgentCancelled -> assertFailure "should not be cancelled"
          Right _ -> pure () -- any non-cancelled result is fine
    ]

-- | A tool-use response as Fable-class models return it: the text the
-- model wrote before the tool call arrives as a progress-update thinking
-- block (with text only when the request asked for @display: updates@),
-- not as a text block.
progressBlocks :: Text -> [ContentBlock]
progressBlocks note =
  [ ThinkingBlock "" "sig-reasoning",
    ThinkingBlock note "sig-progress",
    ToolUseBlock (ToolUseId "tu_1") (ToolName "run_command") (object ["command" .= ("git commit" :: Text)])
  ]

decisionTests :: TestTree
decisionTests =
  testGroup
    "Turn decisions"
    [ testCase "progress-update thinking text before a tool call is intermediate text when updates are requested" $ do
        let blocks = progressBlocks "Here is your weekly plan. Two questions: ..."
        case Turn.decide ToolUse blocks of
          Right (Turn.Continue pending) -> do
            length (Turn.pendingCalls pending) @?= 1
            Turn.outputText True (Turn.pendingMessage pending) @?= "Here is your weekly plan. Two questions: ..."
          other -> assertFailure $ "expected tool continuation, got: " <> show other,
      testCase "text blocks and progress updates are delivered in content order" $ do
        let blocks = TextBlock "First" : progressBlocks "Then this"
        case Turn.decide ToolUse blocks of
          Right (Turn.Continue pending) -> Turn.outputText True (Turn.pendingMessage pending) @?= "First\nThen this"
          other -> assertFailure $ "expected tool continuation, got: " <> show other,
      testCase "empty thinking blocks never contribute text" $ do
        let blocks = [ThinkingBlock "" "sig", TextBlock "Only this", ToolUseBlock (ToolUseId "tu_1") (ToolName "t") (object [])]
        case Turn.decide ToolUse blocks of
          Right (Turn.Continue pending) -> Turn.outputText True (Turn.pendingMessage pending) @?= "Only this"
          other -> assertFailure $ "expected tool continuation, got: " <> show other,
      testCase "thinking text is not user-facing unless updates were requested" $ do
        -- Under display: summarized (or budget thinking) non-empty thinking
        -- blocks are reasoning, which must never be sent to the user.
        let blocks = [ThinkingBlock "The user probably wants..." "sig", TextBlock "Sure", ToolUseBlock (ToolUseId "tu_1") (ToolName "t") (object [])]
        case Turn.decide ToolUse blocks of
          Right (Turn.Continue pending) -> Turn.outputText False (Turn.pendingMessage pending) @?= "Sure"
          other -> assertFailure $ "expected tool continuation, got: " <> show other,
      testCase "end_turn response text is unaffected" $ do
        let blocks = [ThinkingBlock "" "sig", TextBlock "Done"]
        case Turn.decide EndTurn blocks of
          Right (Turn.Finish (Turn.Completed message)) -> Turn.outputText True message @?= "Done"
          other -> assertFailure $ "expected Complete, got: " <> show other,
      testCase "the interrupted-response sentinel is not delivered as a progress update" $ do
        -- A max_tokens stop soon after a tool call ends in a progress block
        -- whose text is a fixed placeholder for the unfinished work.
        let blocks = [TextBlock "Checking the calendar", ThinkingBlock "This part of the response was interrupted before it finished." "sig"]
        case Turn.decide MaxTokens blocks of
          Right (Turn.Finish (Turn.Truncated message)) -> Turn.outputText True message @?= "Checking the calendar"
          other -> assertFailure $ "expected truncation, got: " <> show other
    ]

-- Malformed stop states must never turn into successful answers or empty tool loops.
protocolTests :: TestTree
protocolTests =
  testGroup
    "protocol contract"
    [ testCase "scalar tool inputs reject" $ rejects ToolUse [ToolUseBlock (ToolUseId "id") "tool" (encodeValue 1)],
      testCase "empty call IDs reject" $ rejects ToolUse [call ""],
      testCase "unknown stop reason rejects" $ rejects (StopReasonOther "new") [TextBlock "partial"],
      testCase "tool_use without calls rejects" $ rejects ToolUse [TextBlock "waiting"],
      testCase "end_turn with calls rejects" $ rejects EndTurn [call "one"],
      testCase "duplicate call IDs reject" $ rejects ToolUse [call "same", call "same"],
      testCase "max_tokens never requests tool execution" $ case Turn.decide MaxTokens [call "one"] of
        Right (Turn.Finish (Turn.Truncated _)) -> pure ()
        other -> assertFailure (show other)
    ]
  where
    encodeValue :: Int -> Value
    encodeValue = toJSON
    call ident = ToolUseBlock (ToolUseId ident) "tool" (object [])
    rejects stop blocks = case Turn.decide stop blocks of
      Left _ -> pure ()
      other -> assertFailure ("expected protocol rejection, got " <> show other)

-- | Canned API responses, served in order by the fake endpoint.
fakeResponses :: [Value]
fakeResponses =
  [ apiResponse "tool_use" (progressBlocksJson "Filed the research; committing it now."),
    apiResponse "end_turn" [object ["type" .= ("text" :: Text), "text" .= ("Committed." :: Text)]]
  ]
  where
    apiResponse :: Text -> [Value] -> Value
    apiResponse stop content =
      object
        [ "id" .= ("msg_1" :: Text),
          "type" .= ("message" :: Text),
          "role" .= ("assistant" :: Text),
          "content" .= content,
          "stop_reason" .= stop,
          "usage" .= object ["input_tokens" .= (10 :: Int), "output_tokens" .= (5 :: Int)]
        ]
    progressBlocksJson :: Text -> [Value]
    progressBlocksJson note =
      [ object ["type" .= ("thinking" :: Text), "thinking" .= ("" :: Text), "signature" .= ("sig-reasoning" :: Text)],
        object ["type" .= ("thinking" :: Text), "thinking" .= note, "signature" .= ("sig-progress" :: Text)],
        object
          [ "type" .= ("tool_use" :: Text),
            "id" .= ("tu_1" :: Text),
            "name" .= ("run_command" :: Text),
            "input" .= object ["command" .= ("git commit" :: Text)]
          ]
      ]

-- | Run an action against a fake Messages API that serves 'fakeResponses'
-- in order, passing it the endpoint's base URL.
withFakeApi :: (Text -> IO a) -> IO a
withFakeApi = withReplies fakeResponses

withReplies :: [Value] -> (Text -> IO a) -> IO a
withReplies replies action = withHttpReplies (map (status200,) replies) (\url _ -> action url)

withHttpReplies :: [(Status, Value)] -> (Text -> IO Int -> IO a) -> IO a
withHttpReplies replies action = do
  queue <- newIORef replies
  requests <- newIORef (0 :: Int)
  (port, socket) <- openFreePort
  ready <- newEmptyMVar
  let app _req respond = do
        modifyIORef' requests (+ 1)
        next <- atomicModifyIORef' queue $ \case
          [] -> ([], Nothing)
          (r : rest) -> (rest, Just r)
        case next of
          Nothing -> fail "fake API: no more canned responses"
          Just (status, r) -> respond $ responseLBS status [("Content-Type", "application/json")] (encode r)
      settings = setPort port defaultSettings
  bracket
    (forkIO $ putMVar ready () >> runSettingsSocket settings socket app)
    killThread
    (\_ -> takeMVar ready >> action ("http://127.0.0.1:" <> T.pack (show port)) (readIORef requests))

intermediateDeliveryTests :: TestTree
intermediateDeliveryTests =
  testGroup
    "intermediate delivery"
    [ testCase "narration before a tool call reaches onText when display: updates is configured" $
        withFakeApi $ \baseUrl -> do
          delivered <- newIORef ([] :: [Text])
          cfg <- mkTestConfigWith baseUrl (Just (Adaptive (Just EffortHigh) (Just DisplayUpdates))) (pure False)
          let cfg' = cfg {onText = Just (\t -> modifyIORef' delivered (<> [t]))}
          result <- runAgentTurn cfg' [] (ClaudeMessage User [TextBlock "What did you find?"])
          case result of
            AgentSuccess final _ -> final @?= "Committed."
            other -> assertFailure $ "expected AgentSuccess, got: " <> show other
          texts <- readIORef delivered
          texts @?= ["Filed the research; committing it now."]
    ]

-- Whole-turn specifications use a scripted provider and actual tool handles.
turnExecutionTests :: TestTree
turnExecutionTests =
  testGroup
    "turn execution contract"
    [ testCase "tools start together and results retain call order after reverse completion" $
        withReplies [reply "tool_use" [call "a" "first", call "b" "second"], reply "end_turn" [TextBlock "done"]] $ \url -> do
          firstStarted <- newEmptyMVar
          secondFinished <- newEmptyMVar
          seen <- newIORef ([] :: [Text])
          cfg <- mkTestConfigWith url Nothing (pure False)
          let record name = atomicModifyIORef' seen (\names -> (names <> [name], ()))
              registry =
                registerTool (tool "second" (readMVar firstStarted >> record "second" >> putMVar secondFinished ())) $
                  registerTool (tool "first" (putMVar firstStarted () >> readMVar secondFinished >> record "first")) newToolRegistry
          result <- timeout 2000000 (runAgentTurn cfg {registry = registry} [] input)
          readIORef seen >>= (@?= ["second", "first"])
          case result of
            Just (AgentSuccess _ [_, _, ClaudeMessage User results, _]) ->
              results @?= [ToolResultBlock (ToolUseId "a") [ToolResultText "first"] False, ToolResultBlock (ToolUseId "b") [ToolResultText "second"] False]
            other -> assertFailure (show other),
      testCase "a synchronous tool exception leaves its concurrent sibling running" $
        withReplies [reply "tool_use" [call "a" "fails", call "b" "succeeds"], reply "end_turn" [TextBlock "done"]] $ \url -> do
          siblingStarted <- newEmptyMVar
          failed <- newEmptyMVar
          cfg <- mkTestConfigWith url Nothing (pure False)
          let registry =
                registerTool (tool "fails" ((readMVar siblingStarted >> fail "tool failed") `finally` putMVar failed ())) $
                  registerTool (tool "succeeds" (putMVar siblingStarted () >> readMVar failed)) newToolRegistry
          result <- timeout 2000000 (runAgentTurn cfg {registry = registry} [] input)
          case result of
            Just (AgentSuccess _ [_, _, ClaudeMessage User [ToolResultBlock a [ToolResultText err] True, success], _]) -> do
              a @?= ToolUseId "a"
              assertBool "exception becomes its own error result" ("tool failed" `T.isInfixOf` err)
              success @?= ToolResultBlock (ToolUseId "b") [ToolResultText "succeeds"] False
            other -> assertFailure (show other),
      testCase "cancellation during batch notification prevents tool admission" $
        withReplies [reply "tool_use" [call "a" "first", call "b" "second"]] $ \url -> do
          cancelled <- newIORef False
          invoked <- newIORef (0 :: Int)
          cfg <- mkTestConfigWith url Nothing (readIORef cancelled)
          let count = atomicModifyIORef' invoked (\n -> (n + 1, ()))
              registry = registerTool (tool "first" count) (registerTool (tool "second" count) newToolRegistry)
          result <- runAgentTurn cfg {registry = registry, onToolUse = Just (\_ _ -> writeIORef cancelled True)} [] input
          case result of AgentCancelled -> pure (); other -> assertFailure (show other)
          readIORef invoked >>= (@?= 0),
      testCase "turn cancellation waits for cleanup of every running tool" $
        withReplies [reply "tool_use" [call "a" "first", call "b" "second"]] $ \url -> do
          cancelled <- newIORef False
          cfg <- mkTestConfigWith url Nothing (readIORef cancelled)
          withBlockedTools $ \registry started cleaned ->
            Async.withAsync (runAgentTurn cfg {registry = registry} [] input) $ \worker -> do
              timeout 2000000 started >>= (@?= Just ())
              writeIORef cancelled True
              result <- timeout 2000000 (Async.wait worker)
              case result of Just AgentCancelled -> pure (); other -> assertFailure (show other)
              cleaned,
      testCase "external cancellation waits for cleanup of every running tool" $
        withReplies [reply "tool_use" [call "a" "first", call "b" "second"]] $ \url -> do
          cfg <- mkTestConfigWith url Nothing (pure False)
          withBlockedTools $ \registry started cleaned ->
            Async.withAsync (runAgentTurn cfg {registry = registry} [] input) $ \worker -> do
              timeout 2000000 started >>= (@?= Just ())
              timeout 2000000 (Async.cancel worker) >>= (@?= Just ())
              outcome <- Async.waitCatch worker
              case outcome of Left _ -> pure (); Right other -> assertFailure (show other)
              cleaned,
      testCase "exhaustion counts only this turn and returns only its delta" $
        withReplies [reply "tool_use" [call "now" "tool"]] $ \url -> do
          cfg <- mkTestConfigWith url Nothing (pure False)
          let history = [input, ClaudeMessage Assistant [call "old" "tool"], ClaudeMessage User [ToolResultBlock (ToolUseId "old") [] False]]
          result <- runAgentTurn cfg {agentProfile = cfg.agentProfile {maxIterations = literal @1}} history input
          case result of
            AgentExhausted info -> do
              info.iterationsUsed @?= 1
              info.toolsCalled @?= 1
              length info.messages @?= 3
            other -> assertFailure (show other),
      testCase "truncated calls never execute and outcome remains distinct" $
        withReplies [reply "max_tokens" [TextBlock "partial", call "a" "tool"]] $ \url -> do
          executed <- newIORef False
          cfg <- mkTestConfigWith url Nothing (pure False)
          result <- runAgentTurn cfg {registry = registerTool (tool "tool" (writeIORef executed True)) newToolRegistry} [] input
          case result of
            AgentTruncated text delta -> do
              text @?= "partial"
              delta @?= [input, ClaudeMessage Assistant [TextBlock "partial"]]
            other -> assertFailure (show other)
          readIORef executed >>= (@?= False),
      testCase "cancellation during retry backoff prevents another provider request" $
        withHttpReplies [(status429, object []), (status200, reply "end_turn" [TextBlock "must not request"])] $ \url requestCount -> do
          cancelled <- newIORef False
          cfg <- mkTestConfigWith url Nothing (readIORef cancelled)
          result <- runAgentTurn cfg {onRateLimit = Just (\_ _ -> writeIORef cancelled True)} [] input
          case result of
            AgentCancelled -> pure ()
            other -> assertFailure (show other)
          requestCount >>= (@?= 1),
      testCase "synchronous delegates observe parent cancellation between tools and requests" $
        withReplies [reply "tool_use" [call "stop" "stop"]] $ \url -> do
          cfg <- mkTestConfigWith url Nothing (pure False)
          token <- newTVarIO False
          bracket (Tasks.newAsyncTaskStore 60) Tasks.closeTaskStore $ \store -> do
            let registry = registerTool (tool "stop" (atomically (writeTVar token True))) newToolRegistry
            worker <- delegate cfg registry (Tasks.forTurn token store) Nothing
            result <- worker.execute (object ["task" .= ("work" :: Text)])
            case result of
              ToolError message -> assertBool "delegate cancelled" ("[cancelled]" `T.isPrefixOf` message)
              other -> assertFailure (show other),
      testCase "background execution outlives zero result-retention TTL" $
        withReplies [reply "tool_use" [call "attachment" "queue_attachment", call "block" "block"], reply "end_turn" [TextBlock "done"]] $ \url -> do
          cfg <- mkTestConfigWith url Nothing (pure False)
          entered <- newEmptyMVar
          release <- newEmptyMVar
          queued <- newIORef False
          notified <- newIORef False
          bracket (Tasks.newAsyncTaskStore 0) Tasks.closeTaskStore $ \store -> do
            let registry =
                  registerTool (tool "queue_attachment" (writeIORef queued True)) $
                    registerTool (tool "block" (putMVar entered () >> takeMVar release)) newToolRegistry
            worker <- delegate cfg registry store (Just (\_ _ _ -> writeIORef notified True))
            _ <- worker.execute (object ["task" .= ("work" :: Text), "async" .= True])
            timeout 1000000 (takeMVar entered) >>= (@?= Just ())
            readIORef queued >>= (@?= False)
            readIORef notified >>= (@?= False)
            observed <- (Tasks.mkCheckTaskTool store).execute (object [])
            case observed of
              ToolSuccess parts -> assertBool "running task remains alive" ("running" `T.isInfixOf` T.unlines [text | ToolResultText text <- parts])
              other -> assertFailure (show other)
            putMVar release (),
      testCase "malformed compaction retains original history without persisting" $
        withReplies [reply "max_tokens" [TextBlock "incomplete summary"]] $ \url -> do
          persisted <- newIORef False
          cfg <- mkTestConfigWith url Nothing (pure False)
          let history = [input, ClaudeMessage Assistant [TextBlock "answer"], input]
              config = CompactionConfig (literal @1) (ModelRef "anthropic" "test") Nothing (CKeepTurns (literal @1))
          actual <- compactMessages cfg.logger cfg.client config (pure ()) (\_ _ -> pure ()) (\_ -> writeIORef persisted True) history
          actual @?= history
          readIORef persisted >>= (@?= False)
    ]
  where
    withBlockedTools action = do
      firstStarted <- newEmptyMVar
      secondStarted <- newEmptyMVar
      firstCleaned <- newEmptyMVar
      secondCleaned <- newEmptyMVar
      blocked <- newEmptyMVar @()
      let blocking started cleaned = (putMVar started () >> takeMVar blocked) `finally` putMVar cleaned ()
          registry =
            registerTool (tool "first" (blocking firstStarted firstCleaned)) $
              registerTool (tool "second" (blocking secondStarted secondCleaned)) newToolRegistry
      action registry (takeMVar firstStarted >> takeMVar secondStarted) $ do
        tryTakeMVar firstCleaned >>= (@?= Just ())
        tryTakeMVar secondCleaned >>= (@?= Just ())
    delegate :: AgentConfig -> ToolRegistry -> Tasks.AsyncTaskStore -> Maybe (Text -> ToolUseCallback) -> IO Tool
    delegate cfg registry store notify = do
      metrics <- newMetricsStore
      pure (mkDelegateTaskTool cfg.logger cfg.client registry noApprovalChannel cfg.agentProfile Nothing "/tmp" Nothing metrics (AgentPreset Nothing mempty) Map.empty [] notify (Just store))
    input = ClaudeMessage User [TextBlock "hello"]
    call ident name = ToolUseBlock (ToolUseId ident) name (object [])
    tool name action = Tool (ToolSchema name "test" (object [])) (\_ -> action >> pure (ToolSuccess [ToolResultText name.unToolName]))
    reply :: Text -> [ContentBlock] -> Value
    reply stop blocks =
      object
        [ "id" .= ("test" :: Text),
          "stop_reason" .= stop,
          "content" .= blocks,
          "usage" .= object ["input_tokens" .= (1 :: Int), "output_tokens" .= (1 :: Int)]
        ]

perceiveResultImagesTests :: TestTree
perceiveResultImagesTests =
  testGroup
    "perceiveResultImages"
    [ testCase "small valid image part passes through" $ do
        let png = mkPngBytes 100 80
            b64 = TE.decodeUtf8 (B64.encode png)
            result = perceiveResultImages (Just 200) (ToolSuccess [ToolResultText "ok", ToolResultImage "image/png" b64])
        result @?= ToolSuccess [ToolResultText "ok", ToolResultImage "image/png" b64],
      testCase "oversized image part is resized" $ do
        let png = mkPngBytes 400 200
            b64 = TE.decodeUtf8 (B64.encode png)
        case perceiveResultImages (Just 200) (ToolSuccess [ToolResultImage "image/png" b64]) of
          ToolSuccess [ToolResultImage "image/png" b64'] -> do
            assertBool "data should change when resized" (b64' /= b64)
            case B64.decode (TE.encodeUtf8 b64') of
              Left err -> assertFailure $ "Invalid base64 after resize: " <> err
              Right bytes -> assertBool "should be smaller" (BS.length bytes < BS.length png)
          other -> assertFailure $ "Expected resized image part, got: " <> show other,
      testCase "invalid base64 degrades to elided text" $ do
        case perceiveResultImages Nothing (ToolSuccess [ToolResultImage "image/png" "not!base64!"]) of
          ToolSuccess [ToolResultText t] -> assertBool "mentions elision" (T.isInfixOf "image elided" t)
          other -> assertFailure $ "Expected elided text part, got: " <> show other,
      testCase "mislabeled media type is corrected by content sniffing" $ do
        let b64 = TE.decodeUtf8 (B64.encode (mkPngBytes 10 10))
        case perceiveResultImages Nothing (ToolSuccess [ToolResultImage "image/tiff" b64]) of
          ToolSuccess [ToolResultImage "image/png" b64'] -> b64' @?= b64
          other -> assertFailure $ "Expected corrected image part, got: " <> show other,
      testCase "errors pass through untouched" $ do
        let err = ToolError "boom"
        perceiveResultImages (Just 100) err @?= err
    ]

-- | Build a minimal AgentConfig for testing cancellation.
-- Uses a dummy ClaudeClient that will produce an HTTP error if called.
mkTestConfig :: IO Bool -> IO AgentConfig
mkTestConfig = mkTestConfigWith "http://localhost:1" Nothing

-- | Build a minimal AgentConfig against the given provider base URL.
mkTestConfigWith :: Text -> Maybe ThinkingMode -> IO Bool -> IO AgentConfig
mkTestConfigWith baseUrl thinkingMode isCancelled = do
  mgr <- newManager defaultManagerSettings
  let client =
        ClaudeClient
          { manager = mgr,
            providers = Map.singleton "anthropic" (ProviderConfig "anthropic" baseUrl (Just "test-key") AnthropicFormat ImagesEmbedded)
          }
      profile =
        AgentProfile
          { model = ModelRef "anthropic" "test-model",
            thinking = thinkingMode,
            maxIterations = literal @5,
            cache = Nothing,
            maxTokens = literal @1024,
            systemPrompt = [],
            toolSearch = ToolSearchDisabled,
            toolFilter = AllTools,
            permissions = resolvePermissions mempty
          }
      observer =
        AgentObserver
          { onInputEstimate = \_ _ _ _ -> pure (),
            onApiResponse = \_ _ -> pure (),
            onToolCall = \_ -> pure (),
            onCompaction = pure ()
          }
  pure
    AgentConfig
      { logger = LogAction (const (pure ())),
        client = client,
        registry = newToolRegistry,
        requestApproval = noApprovalChannel,
        systemPrompt = Nothing,
        agentProfile = profile,
        observer = observer,
        onRateLimit = Nothing,
        onText = Nothing,
        onToolUse = Nothing,
        onBeforeApiCall = Nothing,
        toolSearch = Nothing,
        pruningConfig = Nothing,
        pruneHorizon = 0,
        outputFormat = Nothing,
        maxImageDimension = Nothing,
        isCancelled = isCancelled
      }
