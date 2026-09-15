module Test.Elwood.Claude.AgentLoop (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.AgentSettings (AgentProfile (..), ModelRef (..), ToolFilter (..), ToolSearchConfig (..))
import Elwood.Claude.AgentLoop (AgentAction (..), AgentConfig (..), AgentResult (..), classifyResponse, perceiveResultImages, runAgentTurn)
import Elwood.Claude.Client (ClaudeClient (..))
import Elwood.Claude.Observer (AgentObserver (..))
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), StopReason (..), ToolName (..), ToolResultPart (..), ToolUseId (..))
import Elwood.Permissions (resolvePermissions)
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Thinking (ThinkingDisplay (..), ThinkingEffort (..), ThinkingMode (..))
import Elwood.Tools.Registry (newToolRegistry)
import Elwood.Tools.Types (ToolResult (..), noApprovalChannel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket, setPort)
import Test.Elwood.TestImage (mkPngBytes)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.AgentLoop"
    [ cancellationTests,
      classifyResponseTests,
      intermediateDeliveryTests,
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

classifyResponseTests :: TestTree
classifyResponseTests =
  testGroup
    "classifyResponse"
    [ testCase "progress-update thinking text before a tool call is intermediate text when updates are requested" $ do
        let blocks = progressBlocks "Here is your weekly plan. Two questions: ..."
        case classifyResponse True ToolUse blocks [] of
          ContinueWithTools toolUses intermediate _ -> do
            length toolUses @?= 1
            intermediate @?= "Here is your weekly plan. Two questions: ..."
          other -> assertFailure $ "expected ContinueWithTools, got: " <> show other,
      testCase "text blocks and progress updates are delivered in content order" $ do
        let blocks = TextBlock "First" : progressBlocks "Then this"
        case classifyResponse True ToolUse blocks [] of
          ContinueWithTools _ intermediate _ -> intermediate @?= "First\nThen this"
          other -> assertFailure $ "expected ContinueWithTools, got: " <> show other,
      testCase "empty thinking blocks never contribute text" $ do
        let blocks = [ThinkingBlock "" "sig", TextBlock "Only this", ToolUseBlock (ToolUseId "tu_1") (ToolName "t") (object [])]
        case classifyResponse True ToolUse blocks [] of
          ContinueWithTools _ intermediate _ -> intermediate @?= "Only this"
          other -> assertFailure $ "expected ContinueWithTools, got: " <> show other,
      testCase "thinking text is not user-facing unless updates were requested" $ do
        -- Under display: summarized (or budget thinking) non-empty thinking
        -- blocks are reasoning, which must never be sent to the user.
        let blocks = [ThinkingBlock "The user probably wants..." "sig", TextBlock "Sure", ToolUseBlock (ToolUseId "tu_1") (ToolName "t") (object [])]
        case classifyResponse False ToolUse blocks [] of
          ContinueWithTools _ intermediate _ -> intermediate @?= "Sure"
          other -> assertFailure $ "expected ContinueWithTools, got: " <> show other,
      testCase "end_turn response text is unaffected" $ do
        let blocks = [ThinkingBlock "" "sig", TextBlock "Done"]
        case classifyResponse True EndTurn blocks [] of
          Complete text _ -> text @?= "Done"
          other -> assertFailure $ "expected Complete, got: " <> show other,
      testCase "the interrupted-response sentinel is not delivered as a progress update" $ do
        -- A max_tokens stop soon after a tool call ends in a progress block
        -- whose text is a fixed placeholder for the unfinished work.
        let blocks = [TextBlock "Checking the calendar", ThinkingBlock "This part of the response was interrupted before it finished." "sig"]
        case classifyResponse True MaxTokens blocks [] of
          TruncatedResponse text _ -> text @?= "Checking the calendar"
          other -> assertFailure $ "expected TruncatedResponse, got: " <> show other
    ]

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
withFakeApi action = do
  queue <- newIORef fakeResponses
  (port, socket) <- openFreePort
  ready <- newEmptyMVar
  let app _req respond = do
        next <- atomicModifyIORef' queue $ \case
          [] -> ([], Nothing)
          (r : rest) -> (rest, Just r)
        case next of
          Nothing -> fail "fake API: no more canned responses"
          Just r -> respond $ responseLBS status200 [("Content-Type", "application/json")] (encode r)
      settings = setPort port defaultSettings
  bracket
    (forkIO $ putMVar ready () >> runSettingsSocket settings socket app)
    killThread
    (\_ -> takeMVar ready >> action ("http://127.0.0.1:" <> T.pack (show port)))

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
            maxIterations = 5,
            cache = Nothing,
            maxTokens = 1024,
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
