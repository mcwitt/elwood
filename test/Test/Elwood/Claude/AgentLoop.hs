module Test.Elwood.Claude.AgentLoop (tests) where

import Colog.Core (LogAction (..))
import Control.Exception (SomeException, try)
import Elwood.AgentSettings (AgentProfile (..), ToolSearchConfig (..))
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient (..))
import Elwood.Claude.Observer (AgentObserver (..))
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..))
import Elwood.Permissions (resolvePermissions)
import Elwood.Tools.Registry (newToolRegistry)
import Elwood.Tools.Types (noApprovalChannel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.AgentLoop"
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

-- | Build a minimal AgentConfig for testing cancellation.
-- Uses a dummy ClaudeClient that will produce an HTTP error if called.
mkTestConfig :: IO Bool -> IO AgentConfig
mkTestConfig isCancelled = do
  mgr <- newManager defaultManagerSettings
  let client =
        ClaudeClient
          { manager = mgr,
            apiKey = "test-key",
            baseUrl = "http://localhost:1" -- unreachable, will error
          }
      profile =
        AgentProfile
          { model = "test-model",
            thinking = Nothing,
            maxIterations = 5,
            cache = Nothing,
            maxTokens = 1024,
            systemPrompt = [],
            toolSearch = ToolSearchDisabled,
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
        isCancelled = isCancelled
      }
