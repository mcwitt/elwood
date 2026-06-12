module Test.Elwood.Claude.AgentLoop (tests) where

import Colog.Core (LogAction (..))
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.AgentSettings (AgentProfile (..), ModelRef (..), ToolFilter (..), ToolSearchConfig (..))
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), perceiveResultImages, runAgentTurn)
import Elwood.Claude.Client (ClaudeClient (..))
import Elwood.Claude.Observer (AgentObserver (..))
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolResultPart (..))
import Elwood.Permissions (resolvePermissions)
import Elwood.Provider (ApiFormat (..), ProviderConfig (..))
import Elwood.Tools.Registry (newToolRegistry)
import Elwood.Tools.Types (ToolResult (..), noApprovalChannel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Elwood.TestImage (mkPngBytes)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.AgentLoop"
    [ cancellationTests,
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
mkTestConfig isCancelled = do
  mgr <- newManager defaultManagerSettings
  let client =
        ClaudeClient
          { manager = mgr,
            providers = Map.singleton "anthropic" (ProviderConfig "anthropic" "http://localhost:1" (Just "test-key") AnthropicFormat)
          }
      profile =
        AgentProfile
          { model = ModelRef "anthropic" "test-model",
            thinking = Nothing,
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
