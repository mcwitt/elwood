module Test.Elwood.Tools.Delegate (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Either (fromRight)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.AgentSettings (AgentOverrides, AgentPreset (..), AgentProfile (..), ModelRef (..), ToolFilter (..), ToolSearchConfig (..), resolveProfile)
import Elwood.AgentSettings qualified as Override (AgentOverrides (..))
import Elwood.Claude.Client (newClient)
import Elwood.Claude.Types (ContentBlock (..), ToolName (..), ToolResultPart (..), ToolSchema (..))
import Elwood.Logging (LogMsg (..))
import Elwood.Metrics (estimateTextTokens, estimateToolTokens, newMetricsStore)
import Elwood.Permissions (defaultPermissionConfig)
import Elwood.Positive (literal)
import Elwood.Prompt (PromptInput (InlineText))
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Thinking (ThinkingDisplay (..), ThinkingEffort (..), ThinkingMode (..))
import Elwood.Tools.AsyncTask qualified as Tasks
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Delegate (inheritedOverrides, mkDelegateTaskTool)
import Elwood.Tools.Registry (registerTool)
import Elwood.Tools.Types
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty
import Test.Tasty.HUnit

hasProperty :: Text -> Value -> Bool
hasProperty k (Object o) = case KM.lookup "properties" o of
  Just (Object props) -> KM.member (Key.fromText k) props
  _ -> False
hasProperty _ _ = False

tests :: TestTree
tests =
  testGroup
    "Tools.Delegate"
    [ schemaTests,
      inputParsingTests,
      modelValidationTests,
      thinkingValidationTests,
      inheritanceTests,
      maxIterationsValidationTests,
      agentSelectionTests,
      systemPromptValidationTests,
      asyncValidationTests,
      timeoutValidationTests,
      labelValidationTests,
      toolsValidationTests,
      toolSelectionTests,
      descriptionTests
    ]

schemaTests :: TestTree
schemaTests =
  testGroup
    "Schema"
    [ testCase "tool name is delegate_task" $ do
        let tool = mkStubDelegateTool
        tool.schema.name @?= ToolName "delegate_task",
      testCase "description is non-empty" $ do
        let tool = mkStubDelegateTool
        assertBool "description should not be empty" (tool.schema.description /= ("" :: Text)),
      testCase "schema advertises a tools property" $ do
        let tool = mkStubDelegateTool
        assertBool "tools property present" (hasProperty "tools" tool.schema.inputSchema),
      testCase "tools schema describes string items and restricts string alternative to all" $
        property "anyOf" (property "tools" (property "properties" mkStubDelegateTool.schema.inputSchema))
          @?= Aeson.toJSON
            [ object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]],
              object ["type" .= ("string" :: Text), "enum" .= ["all" :: Text]]
            ]
    ]

inputParsingTests :: TestTree
inputParsingTests =
  testGroup
    "Input parsing"
    [ testCase "missing task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object [])
        assertToolError result,
      testCase "empty task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("  " :: Text)])
        assertToolError result,
      testCase "non-string task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= (42 :: Int)])
        assertToolError result,
      testCase "non-object input returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (String "not an object")
        assertToolError result
    ]

modelValidationTests :: TestTree
modelValidationTests =
  testGroup
    "Model validation"
    [ testCase "invalid model returns error" $ do
        let tool = mkStubDelegateToolWithModels ["model-a", "model-b"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= ("bad-model" :: Text)])
        assertToolError result,
      testCase "non-string model returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= (42 :: Int)])
        assertToolError result,
      testCase "model rejected when allowed_models is empty" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= ("any-model" :: Text)])
        assertToolError result
    ]

inheritanceTests :: TestTree
inheritanceTests =
  testGroup
    "inheritedOverrides"
    [ testCase "thinking display is not inherited by sub-agents" $ do
        let parent = parentProfile (Just (Adaptive (Just EffortHigh) (Just DisplayUpdates)))
        inheritedThinking parent @?= Just (Adaptive (Just EffortHigh) Nothing),
      testCase "other thinking settings are inherited unchanged" $ do
        let budget = parentProfile (Just (Budget (literal @4096)))
            off = parentProfile Nothing
        inheritedThinking budget @?= Just (Budget (literal @4096))
        inheritedThinking off @?= Nothing
    ]
  where
    inheritedThinking = (.thinking) . resolveProfile . inheritedOverrides
    parentProfile thinking =
      AgentProfile
        (ModelRef "anthropic" "model-x")
        thinking
        (literal @15)
        Nothing
        (literal @32768)
        [InlineText "test"]
        ToolSearchDisabled
        AllTools
        defaultPermissionConfig

thinkingValidationTests :: TestTree
thinkingValidationTests =
  testGroup
    "Thinking validation"
    [ testCase "string thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= ("medium" :: Text)])
        assertToolError result,
      testCase "non-object thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= (42 :: Int)])
        assertToolError result,
      testCase "array thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= [True]])
        assertToolError result
    ]

maxIterationsValidationTests :: TestTree
maxIterationsValidationTests =
  testGroup
    "max_iterations validation"
    [ testCase "non-integer returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= ("five" :: Text)])
        assertToolError result,
      testCase "too large returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= (100 :: Int)])
        assertToolError result,
      testCase "zero returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= (0 :: Int)])
        assertToolError result
    ]

agentSelectionTests :: TestTree
agentSelectionTests =
  testGroup
    "Agent selection"
    [ testCase "agent rejected when extra_agents is empty" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= ("fast" :: Text)])
        assertToolError result,
      testCase "invalid agent name returns error" $ do
        let tool = mkStubDelegateToolWithAgents ["fast", "deep"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= ("turbo" :: Text)])
        assertToolError result,
      testCase "non-string agent returns error" $ do
        let tool = mkStubDelegateToolWithAgents ["fast"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= (42 :: Int)])
        assertToolError result
    ]

systemPromptValidationTests :: TestTree
systemPromptValidationTests =
  testGroup
    "system_prompt validation"
    [ testCase "non-string system_prompt returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "system_prompt" .= (42 :: Int)])
        assertToolError result,
      testCase "empty system_prompt returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "system_prompt" .= ("  " :: Text)])
        assertToolError result
    ]

asyncValidationTests :: TestTree
asyncValidationTests =
  testGroup
    "async validation"
    [ testCase "non-boolean async returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "async" .= ("yes" :: Text)])
        assertToolError result,
      testCase "async without store returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "async" .= True])
        assertToolError result
    ]

timeoutValidationTests :: TestTree
timeoutValidationTests =
  testGroup
    "timeout_seconds validation"
    [ testCase "non-integer timeout_seconds returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "timeout_seconds" .= ("five" :: Text)])
        assertToolError result,
      testCase "zero timeout_seconds returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "timeout_seconds" .= (0 :: Int)])
        assertToolError result,
      testCase "negative timeout_seconds returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "timeout_seconds" .= (-1 :: Int)])
        assertToolError result
    ]

labelValidationTests :: TestTree
labelValidationTests =
  testGroup
    "label validation"
    [ testCase "non-string label returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "label" .= (42 :: Int)])
        assertToolError result,
      testCase "empty label returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "label" .= ("  " :: Text)])
        assertToolError result
    ]

toolsValidationTests :: TestTree
toolsValidationTests =
  testGroup
    "tools validation"
    [ testCase "non-string/array tools returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("x" :: Text), "tools" .= (5 :: Int)])
        assertToolError result
    ]

-- The provider's tool-use JSON is decoded before the delegate receives it.
-- Observe the actual outgoing request to test selection beyond input parsing.
toolSelectionTests :: TestTree
toolSelectionTests =
  testGroup
    "tool selection"
    [ testCase "array from a model response selects only the requested tool" $
        withDelegate ToolSearchDisabled 0 $ \tool requests _ -> do
          let wire = object ["type" .= ("tool_use" :: Text), "id" .= ("delegate" :: Text), "name" .= ("delegate_task" :: Text), "input" .= arguments ["tools" .= ["run_command" :: Text]]]
          case Aeson.eitherDecode @ContentBlock (Aeson.encode wire) of
            Right (ToolUseBlock _ _ input) -> tool.execute input >>= (@?= toolSuccess "done")
            other -> assertFailure (show other)
          sent <- requests
          map toolNames sent @?= [["run_command"]],
      testCase "omitting tools preserves the named preset" $
        withDelegate ToolSearchDisabled 0 $ \tool requests _ -> do
          tool.execute (arguments []) >>= (@?= toolSuccess "done")
          requests >>= (\sent -> map toolNames sent @?= [["run_command"]]),
      testCase "empty tools array removes every tool" $
        withDelegate ToolSearchDisabled 0 $ \tool requests _ -> do
          tool.execute (arguments ["tools" .= ([] :: [Text])]) >>= (@?= toolSuccess "done")
          requests >>= (\sent -> map toolNames sent @?= [[]]),
      testCase "all explicitly overrides preset and reports eager catalog cost" $
        withDelegate ToolSearchDisabled 0 $ \tool requests logs -> do
          tool.execute (arguments ["tools" .= ("all" :: Text)]) >>= (@?= toolSuccess "done")
          requests >>= (\sent -> map toolNames sent @?= [["large", "run_command"]])
          entries <- logs
          let expected = catalogEstimate ToolSearchDisabled
          assertBool "full catalog estimate logged" (any (elem ("catalog", expected) . (.context)) entries),
      testCase "all preserves configured search and estimates only upfront schemas" $
        withDelegate (ToolSearchEnabled ["run_command"]) 0 $ \tool requests logs -> do
          tool.execute (arguments ["tools" .= ("all" :: Text)]) >>= (@?= toolSuccess "done")
          sent <- requests
          case sent of
            [request] -> do
              toolNames request @?= ["tool_search_tool_bm25", "large", "run_command"]
              case property "tools" request of
                Array schemas -> do
                  let deferred = [(property "name" schema, property "defer_loading" schema) | schema <- foldr (:) [] schemas]
                  assertBool "large schema deferred" ((String "large", Bool True) `elem` deferred)
                  assertBool "run_command upfront" ((String "run_command", Bool False) `elem` deferred)
                other -> assertFailure (show other)
            other -> assertFailure (show other)
          entries <- logs
          assertBool "deferred schema excluded from upfront estimate" (any (elem ("catalog", catalogEstimate (ToolSearchEnabled ["run_command"])) . (.context)) entries),
      testCase "encoded arrays get an actionable error without a model request" $
        withDelegate ToolSearchDisabled 0 $ \tool requests _ -> do
          result <- tool.execute (arguments ["tools" .= ("[\"run_command\"]" :: Text)])
          case result of
            ToolError message -> assertBool "explains expected JSON shape" ("tools must be an array of tool names" `T.isInfixOf` message)
            other -> assertFailure (show other)
          requests >>= (@?= []),
      testCase "non-string array elements fail before a model request" $
        withDelegate ToolSearchDisabled 0 $ \tool requests _ -> do
          tool.execute (arguments ["tools" .= [Number 42]]) >>= assertToolError
          requests >>= (@?= []),
      testCase "timeout reports initial catalog size and actionable selection guidance" $
        withDelegate ToolSearchDisabled 2000000 $ \tool _ _ -> do
          result <- tool.execute (arguments ["tools" .= ("all" :: Text), "timeout_seconds" .= (1 :: Int)])
          case result of
            ToolError message -> do
              assertBool "catalog size and token estimate" (catalogEstimate ToolSearchDisabled `T.isInfixOf` message)
              assertBool "smaller catalog guidance" ("pass a smaller tools array" `T.isInfixOf` message)
            other -> assertFailure (show other),
      testCase "background admission reports initial catalog cost" $
        withDelegate ToolSearchDisabled 0 $ \tool _ _ -> do
          result <- tool.execute (arguments ["tools" .= ("all" :: Text), "async" .= True])
          case result of
            ToolSuccess parts -> assertBool "cost visible before task completes" (any (\case ToolResultText message -> catalogEstimate ToolSearchDisabled `T.isInfixOf` message; _ -> False) parts)
            other -> assertFailure (show other)
    ]
  where
    arguments fields = object (["agent" .= ("local" :: Text), "task" .= ("test task" :: Text)] <> fields)

property :: Aeson.Key -> Value -> Value
property key (Object fields) = fromMaybe Null (KM.lookup key fields)
property _ _ = Null

toolNames :: Value -> [Value]
toolNames request = case property "tools" request of
  Array schemas -> map (property "name") (foldr (:) [] schemas)
  _ -> []

largeTool :: Tool
largeTool = Tool (ToolSchema "large" (T.replicate 4000 "large catalog ") (object [])) (\_ -> pure (toolSuccess "unused"))

catalogEstimate :: ToolSearchConfig -> Text
catalogEstimate search =
  "2 tools (" <> T.pack (show (length eager)) <> " upfront); approximately " <> T.pack (show tokens) <> " initial input tokens"
  where
    eager = case search of
      ToolSearchDisabled -> [largeTool.schema, command.schema]
      ToolSearchEnabled _ -> [command.schema]
    tokens = estimateTextTokens "system" + estimateTextTokens "test task" + sum (map estimateToolTokens eager)
    command = mkRunCommandTool (LogAction (const (pure ()))) "/tmp" defaultPermissionConfig

withDelegate :: ToolSearchConfig -> Int -> (Tool -> IO [Value] -> IO [LogMsg] -> Assertion) -> Assertion
withDelegate search delay action = do
  requests <- newIORef []
  logs <- newIORef []
  let append ref value = atomicModifyIORef' ref (\values -> (values <> [value], ()))
      logger = LogAction (append logs)
      response = object ["id" .= ("test" :: Text), "stop_reason" .= ("end_turn" :: Text), "content" .= [TextBlock "done"], "usage" .= object ["input_tokens" .= (1 :: Int), "output_tokens" .= (1 :: Int)]]
      application request respond = do
        body <- strictRequestBody request
        append requests (fromRight Null (Aeson.eitherDecode body))
        threadDelay delay
        respond (responseLBS status200 [] (Aeson.encode response))
  testWithApplication (pure application) $ \port -> do
    client <- newClient (Map.singleton "local" (ProviderConfig "local" ("http://127.0.0.1:" <> T.pack (show port)) Nothing AnthropicFormat ImagesEmbedded))
    metrics <- newMetricsStore
    let parent = (resolveProfile mempty) {model = ModelRef "local" "test", systemPrompt = [InlineText "system"]}
        preset = AgentPreset Nothing ((mempty :: AgentOverrides) {Override.toolFilter = Last (Just (OnlyTools (Set.singleton "run_command"))), Override.toolSearch = Last (Just search)})
    bracket (Tasks.newAsyncTaskStore 0) Tasks.closeTaskStore $ \store -> do
      let tool = mkDelegateTaskTool logger client (registerTool largeTool mempty) noApprovalChannel parent Nothing "/tmp" Nothing metrics (AgentPreset Nothing mempty) (Map.singleton "local" preset) [] Nothing (Just store)
      action tool (readIORef requests) (readIORef logs)

descriptionTests :: TestTree
descriptionTests =
  testGroup
    "Descriptions"
    [ testCase "preset descriptions appear in agent parameter description" $ do
        let presets =
              Map.fromList
                [ ("fast", AgentPreset (Just "Quick Haiku responses") mempty),
                  ("deep", AgentPreset (Just "Extended thinking for complex analysis") mempty)
                ]
            tool =
              mkDelegateTaskTool
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                (AgentPreset Nothing mempty)
                presets
                []
                Nothing
                Nothing
            agentDesc = extractAgentDescription tool.schema.inputSchema
        assertBool "should contain 'fast'" (T.isInfixOf "fast" agentDesc)
        assertBool "should contain fast description" (T.isInfixOf "Quick Haiku responses" agentDesc)
        assertBool "should contain 'deep'" (T.isInfixOf "deep" agentDesc)
        assertBool "should contain deep description" (T.isInfixOf "Extended thinking for complex analysis" agentDesc),
      testCase "default agent description appears in tool description" $ do
        let tool =
              mkDelegateTaskTool
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                (AgentPreset (Just "General-purpose sub-agent") mempty)
                Map.empty
                []
                Nothing
                Nothing
        assertBool "tool description should contain default agent description" $
          T.isInfixOf "General-purpose sub-agent" tool.schema.description,
      testCase "preset without description shows just the name" $ do
        let presets = Map.fromList [("fast", AgentPreset Nothing mempty)]
            tool =
              mkDelegateTaskTool
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                (AgentPreset Nothing mempty)
                presets
                []
                Nothing
                Nothing
            agentDesc = extractAgentDescription tool.schema.inputSchema
        assertBool "should contain 'fast'" (T.isInfixOf "fast" agentDesc)
        -- Should not have parentheses for a description-less preset
        assertBool "should not have parens" (not (T.isInfixOf "(" agentDesc))
    ]

-- | Extract the agent parameter description from the input schema
extractAgentDescription :: Value -> Text
extractAgentDescription (Aeson.Object schema) =
  case KM.lookup "properties" schema of
    Just (Aeson.Object props) ->
      case KM.lookup "agent" props of
        Just (Aeson.Object agentObj) ->
          case KM.lookup "description" agentObj of
            Just (Aeson.String d) -> d
            _ -> ""
        _ -> ""
    _ -> ""
extractAgentDescription _ = ""

-- | Build a delegate tool that will fail at the API call level but can
-- validate input parsing. Uses undefined for fields only needed at
-- runtime (logger, client, registry, etc.) since input validation
-- happens before they are accessed.
mkStubDelegateTool :: Tool
mkStubDelegateTool = mkStubDelegateToolWithModels []

mkStubDelegateToolWithModels :: [Text] -> Tool
mkStubDelegateToolWithModels models =
  mkDelegateTaskTool
    undefined -- logger
    undefined -- client
    undefined -- baseRegistry
    undefined -- context
    undefined -- parentProfile
    undefined -- pruning
    undefined -- workspace
    undefined -- maxImageDim
    undefined -- metrics
    (AgentPreset Nothing mempty) -- delegateAgentPreset
    Map.empty -- extraAgents
    models
    Nothing -- delegateOnToolUse
    Nothing -- asyncStore

mkStubDelegateToolWithAgents :: [Text] -> Tool
mkStubDelegateToolWithAgents agentNames =
  mkDelegateTaskTool
    undefined -- logger
    undefined -- client
    undefined -- baseRegistry
    undefined -- context
    undefined -- parentProfile
    undefined -- pruning
    undefined -- workspace
    undefined -- maxImageDim
    undefined -- metrics
    (AgentPreset Nothing mempty) -- delegateAgentPreset
    (Map.fromList [(n, AgentPreset Nothing mempty) | n <- agentNames])
    [] -- allowedModels
    Nothing -- delegateOnToolUse
    Nothing -- asyncStore

-- Parsing promises a tool error; explanatory prose is not a wire contract.
assertToolError :: ToolResult -> Assertion
assertToolError (ToolError message) = assertBool "error explains the failure" (not (T.null message))
assertToolError other = assertFailure ("expected tool error, got " <> show other)
