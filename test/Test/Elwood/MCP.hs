module Test.Elwood.MCP (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Elwood.Claude.Types (ToolResultPart (..))
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (newLogger)
import Elwood.Logging qualified as Log
import Elwood.MCP.Client (defaultRequestTimeoutSeconds, sendRequest, spawnServer, stopServer)
import Elwood.MCP.Registry
  ( extractTimeout,
    injectTimeoutProperty,
    maxRequestTimeoutSeconds,
    normalizeInputSchema,
    schemaDeclaresTimeout,
    toolResultParts,
  )
import Elwood.MCP.Types
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "MCP"
    [ jsonRpcTests,
      mcpToolTests,
      mcpErrorTests,
      mcpServerConfigTests,
      concurrentRequestTests,
      schemaNormalizationTests,
      timeoutArgTests,
      toolResultPartsTests
    ]

schemaNormalizationTests :: TestTree
schemaNormalizationTests =
  testGroup
    "schema normalization"
    [ testCase "flattens a top-level action union" $ do
        let schema =
              object
                [ "anyOf"
                    .= [ object
                           [ "type" .= ("object" :: T.Text),
                             "properties"
                               .= object
                                 [ "action" .= object ["type" .= ("string" :: T.Text), "const" .= ("search" :: T.Text)],
                                   "query" .= object ["type" .= ("string" :: T.Text)],
                                   "groups" .= object ["type" .= ("array" :: T.Text), "items" .= object ["type" .= ("string" :: T.Text)]]
                                 ],
                             "required" .= (["action", "query"] :: [T.Text]),
                             "additionalProperties" .= False
                           ],
                         object
                           [ "type" .= ("object" :: T.Text),
                             "properties"
                               .= object
                                 [ "action" .= object ["type" .= ("string" :: T.Text), "const" .= ("disable" :: T.Text)],
                                   "groups" .= object ["type" .= ("array" :: T.Text), "items" .= object ["$ref" .= ("#/anyOf/0/properties/groups/items" :: T.Text)]]
                                 ],
                             "required" .= (["action"] :: [T.Text]),
                             "additionalProperties" .= False
                           ]
                       ]
                ]
        case normalizeInputSchema schema of
          Object normalized -> do
            KM.lookup "type" normalized @?= Just (String "object")
            mapM_ (\key -> assertBool ("top-level combinator was removed: " <> show key) (not (KM.member key normalized))) ["oneOf", "allOf", "anyOf"]
            KM.lookup "required" normalized @?= Just (toJSON (["action"] :: [T.Text]))
            KM.lookup "additionalProperties" normalized @?= Just (Bool False)
            case KM.lookup "properties" normalized of
              Just (Object properties) -> do
                assertBool "query property retained" (KM.member "query" properties)
                case KM.lookup "action" properties of
                  Just (Object action) -> assertBool "action alternatives retained below the root" (KM.member "anyOf" action)
                  _ -> assertFailure "Expected merged action property"
                case KM.lookup "groups" properties of
                  Just (Object groups) -> case KM.lookup "items" groups of
                    Just (Object items) -> assertBool "local reference expanded" (not (KM.member "$ref" items))
                    _ -> assertFailure "Expected groups item schema"
                  _ -> assertFailure "Expected groups property"
              _ -> assertFailure "Expected merged properties"
          _ -> assertFailure "Expected object schema",
      testCase "flattens all prohibited top-level combinators" $ do
        let branch = object ["type" .= ("object" :: T.Text), "properties" .= object ["value" .= object ["type" .= ("string" :: T.Text)]]]
        mapM_
          ( \key -> case normalizeInputSchema (object [key .= [branch]]) of
              Object normalized -> do
                assertBool "combinator removed" (not (KM.member key normalized))
                case KM.lookup "properties" normalized of
                  Just (Object properties) -> assertBool "branch property retained" (KM.member "value" properties)
                  _ -> assertFailure "Expected properties"
              _ -> assertFailure "Expected object schema"
          )
          ["oneOf", "allOf", "anyOf"]
    ]

-- | Tests for converting MCP tool results into tool result parts
toolResultPartsTests :: TestTree
toolResultPartsTests =
  testGroup
    "toolResultParts"
    [ testCase "text-only content yields a single merged text part" $ do
        let result =
              object
                [ "content"
                    .= [ object ["type" .= ("text" :: T.Text), "text" .= ("line one" :: T.Text)],
                         object ["type" .= ("text" :: T.Text), "text" .= ("line two" :: T.Text)]
                       ]
                ]
        toolResultParts result @?= [ToolResultText "line one\nline two"],
      testCase "image content becomes an image part (read_media_file shape)" $ do
        let result =
              object
                [ "content"
                    .= [ object
                           [ "type" .= ("image" :: T.Text),
                             "data" .= ("aGVsbG8=" :: T.Text),
                             "mimeType" .= ("image/png" :: T.Text)
                           ]
                       ]
                ]
        toolResultParts result @?= [ToolResultImage "image/png" "aGVsbG8="],
      testCase "mixed content preserves order, merging only adjacent text" $ do
        let result =
              object
                [ "content"
                    .= [ object ["type" .= ("text" :: T.Text), "text" .= ("before" :: T.Text)],
                         object
                           [ "type" .= ("image" :: T.Text),
                             "data" .= ("aW1n" :: T.Text),
                             "mimeType" .= ("image/jpeg" :: T.Text)
                           ],
                         object ["type" .= ("text" :: T.Text), "text" .= ("after" :: T.Text)]
                       ]
                ]
        toolResultParts result
          @?= [ ToolResultText "before",
                ToolResultImage "image/jpeg" "aW1n",
                ToolResultText "after"
              ],
      testCase "image block missing data falls back to text" $ do
        let result =
              object
                ["content" .= [object ["type" .= ("image" :: T.Text), "mimeType" .= ("image/png" :: T.Text)]]]
        case toolResultParts result of
          [ToolResultText _] -> pure ()
          other -> assertFailure $ "Expected single text part, got: " <> show other,
      testCase "non-object result renders as text" $
        toolResultParts (toJSON ("plain string" :: T.Text)) @?= [ToolResultText "plain string"]
    ]

jsonRpcTests :: TestTree
jsonRpcTests =
  testGroup
    "JSON-RPC"
    [ testCase "JsonRpcRequest serializes correctly" $ do
        let req =
              JsonRpcRequest
                { jsonrpc = "2.0",
                  method = "tools/list",
                  params = Nothing,
                  id_ = 1
                }
        let jsonVal = toJSON req
        case jsonVal of
          Object obj -> do
            KM.lookup "jsonrpc" obj @?= Just (String "2.0")
            KM.lookup "method" obj @?= Just (String "tools/list")
            KM.lookup "id" obj @?= Just (Number 1)
            KM.member "params" obj @?= False
          _ -> assertFailure "Expected object",
      testCase "JsonRpcRequest with params serializes correctly" $ do
        let params_ = object ["name" .= ("test" :: String)]
        let req =
              JsonRpcRequest
                { jsonrpc = "2.0",
                  method = "tools/call",
                  params = Just params_,
                  id_ = 42
                }
        let jsonVal = toJSON req
        case jsonVal of
          Object obj -> do
            KM.lookup "method" obj @?= Just (String "tools/call")
            KM.lookup "id" obj @?= Just (Number 42)
            KM.member "params" obj @?= True
          _ -> assertFailure "Expected object",
      testCase "JsonRpcResponse parses success" $ do
        let jsonStr = "{\"jsonrpc\":\"2.0\",\"result\":{\"tools\":[]},\"id\":1}"
        case eitherDecode jsonStr :: Either String JsonRpcResponse of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right resp -> do
            resp.jsonrpc @?= "2.0"
            resp.id_ @?= Just 1
            resp.error @?= Nothing
            case resp.result of
              Just (Object _) -> pure ()
              _ -> assertFailure "Expected result object",
      testCase "JsonRpcResponse parses error" $ do
        let jsonStr =
              "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":1}"
        case eitherDecode jsonStr :: Either String JsonRpcResponse of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right resp -> do
            resp.result @?= Nothing
            case resp.error of
              Just rpcErr -> do
                rpcErr.code @?= (-32600)
                rpcErr.message @?= "Invalid Request"
              Nothing -> assertFailure "Expected error",
      testCase "JsonRpcError parses with data" $ do
        let jsonStr = "{\"code\":-32000,\"message\":\"Server error\",\"data\":{\"detail\":\"info\"}}"
        case eitherDecode jsonStr :: Either String JsonRpcError of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right rpcErr -> do
            rpcErr.code @?= (-32000)
            rpcErr.message @?= "Server error"
            case rpcErr.data_ of
              Just (Object _) -> pure ()
              _ -> assertFailure "Expected data object"
    ]

mcpToolTests :: TestTree
mcpToolTests =
  testGroup
    "MCPTool"
    [ testCase "parses tool definition" $ do
        let jsonStr =
              "{\"name\":\"read_file\",\"description\":\"Read a file\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}}}}"
        case eitherDecode jsonStr :: Either String MCPTool of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tool -> do
            tool.name @?= "read_file"
            tool.description @?= Just "Read a file"
            case tool.inputSchema of
              Object obj -> KM.member "properties" obj @?= True
              _ -> assertFailure "Expected schema object",
      testCase "parses tool without description" $ do
        let jsonStr = "{\"name\":\"simple_tool\",\"inputSchema\":{\"type\":\"object\"}}"
        case eitherDecode jsonStr :: Either String MCPTool of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tool -> do
            tool.name @?= "simple_tool"
            tool.description @?= Nothing
    ]

mcpErrorTests :: TestTree
mcpErrorTests =
  testGroup
    "MCPError"
    [ testCase "MCPSpawnError has descriptive show" $ do
        let err = MCPSpawnError "command not found"
        show err @?= "MCPSpawnError \"command not found\"",
      testCase "MCPToolError contains code and message" $ do
        let err = MCPToolError (-32000) "Tool failed"
        case err of
          MCPToolError code msg -> do
            code @?= (-32000)
            msg @?= "Tool failed"
          _ -> assertFailure "Expected MCPToolError",
      testCase "MCPError equality works" $ do
        MCPSpawnError "foo" @?= MCPSpawnError "foo"
        MCPSpawnError "foo" /= MCPSpawnError "bar" @?= True
        MCPToolError 1 "a" /= MCPInitializeError "a" @?= True
    ]

mcpServerConfigTests :: TestTree
mcpServerConfigTests =
  testGroup
    "MCPServerConfig"
    [ testCase "can create config with all fields" $ do
        let config =
              MCPServerConfig
                { name = "filesystem",
                  command = "npx",
                  args = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
                  env = Just [("DEBUG", "true")],
                  startupDelay = 2000
                }
        config.name @?= "filesystem"
        config.command @?= "npx"
        length config.args @?= 3
        config.env @?= Just [("DEBUG", "true")]
        config.startupDelay @?= 2000,
      testCase "can create config without env" $ do
        let config =
              MCPServerConfig
                { name = "simple",
                  command = "my-server",
                  args = [],
                  env = Nothing,
                  startupDelay = 0
                }
        config.env @?= Nothing
        config.startupDelay @?= 0
    ]

-- | Regression test for issue #33: concurrent sendRequest calls to the same
-- MCP server must each receive their own response. Before the fix, threads
-- raced on reading stdout directly, causing "Response ID mismatch" errors.
concurrentRequestTests :: TestTree
concurrentRequestTests =
  testGroup
    "Concurrent Requests"
    [ testCase "concurrent requests dispatched correctly" $ do
        logger <- newLogger Log.Error
        withSystemTempFile "fake-mcp.sh" $ \scriptPath scriptH -> do
          -- Fake MCP server that responds out of order: higher IDs respond
          -- first (shorter delay) so the first thread blocked on hGetLine
          -- reads a response meant for a later thread.
          hPutStr scriptH $
            unlines
              [ "while IFS= read -r line; do",
                "  id=$(echo \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
                "  if [ -n \"$id\" ]; then",
                "    if [ \"$id\" -le 1 ]; then",
                "      printf '{\"jsonrpc\":\"2.0\",\"result\":{\"echo_id\":%d},\"id\":%d}\\n' \"$id\" \"$id\"",
                "    else",
                "      {",
                "        sleep \"0.$(printf '%03d' $((150 - id * 10)))\"",
                "        printf '{\"jsonrpc\":\"2.0\",\"result\":{\"echo_id\":%d},\"id\":%d}\\n' \"$id\" \"$id\"",
                "      } &",
                "    fi",
                "  fi",
                "done"
              ]
          hClose scriptH

          let cfg =
                MCPServerConfig
                  { name = "test-echo",
                    command = "bash",
                    args = [T.pack scriptPath],
                    env = Nothing,
                    startupDelay = 0
                  }

          serverResult <- spawnServer logger cfg
          case serverResult of
            Left err -> assertFailure $ "spawnServer failed: " ++ show err
            Right server -> flip finally (stopServer server) $ do
              let n = 10 :: Int
              mvars <-
                mapM
                  ( \i -> do
                      mv <- newEmptyMVar
                      _ <- forkIO $ do
                        r <- sendRequest server defaultRequestTimeoutSeconds "echo" (Just (object ["n" .= i]))
                        putMVar mv r
                      pure mv
                  )
                  [1 .. n]
              results <- mapM takeMVar mvars
              mapM_
                ( \(i, r) -> case r of
                    Left err ->
                      assertFailure $ "Request " ++ show i ++ " failed: " ++ show err
                    Right _ -> pure ()
                )
                (zip [(1 :: Int) ..] results)
    ]

-- | Tests for the per-call @timeout_seconds@ argument injected into every
-- MCP-bridged tool (issue #50).
timeoutArgTests :: TestTree
timeoutArgTests =
  testGroup
    "timeout_seconds argument"
    [ testCase "default timeout when arg missing" $
        extractTimeout (object ["url" .= ("https://example.com" :: T.Text)])
          @?= Right (defaultRequestTimeoutSeconds, object ["url" .= ("https://example.com" :: T.Text)]),
      testCase "explicit timeout extracted and stripped from args" $
        extractTimeout (object ["url" .= ("u" :: T.Text), "timeout_seconds" .= (90 :: Int)])
          @?= Right (90, object ["url" .= ("u" :: T.Text)]),
      testCase "timeout above max is clamped" $
        extractTimeout (object ["timeout_seconds" .= (10000 :: Int)])
          @?= Right (maxRequestTimeoutSeconds, object []),
      testCase "timeout below 1 is clamped to 1" $
        extractTimeout (object ["timeout_seconds" .= (0 :: Int)])
          @?= Right (1, object []),
      testCase "non-integer timeout returns error" $
        extractTimeout (object ["timeout_seconds" .= ("ten" :: T.Text)])
          @?= Left "Invalid 'timeout_seconds' parameter (must be an integer)",
      testCase "schema injection adds property to empty schema" $ do
        let augmented = injectTimeoutProperty (object ["type" .= ("object" :: T.Text)])
        case augmented of
          Object obj -> case KM.lookup "properties" obj of
            Just (Object props) ->
              KM.member "timeout_seconds" props @?= True
            _ -> assertFailure "expected properties object"
          _ -> assertFailure "expected augmented object",
      testCase "schema injection preserves existing properties" $ do
        let original =
              object
                [ "type" .= ("object" :: T.Text),
                  "properties" .= object ["url" .= object ["type" .= ("string" :: T.Text)]]
                ]
        case injectTimeoutProperty original of
          Object obj -> case KM.lookup "properties" obj of
            Just (Object props) -> do
              KM.member "url" props @?= True
              KM.member "timeout_seconds" props @?= True
            _ -> assertFailure "expected properties object"
          _ -> assertFailure "expected augmented object",
      testCase "schemaDeclaresTimeout detects upstream property" $ do
        schemaDeclaresTimeout (object ["properties" .= object ["timeout_seconds" .= object []]])
          @?= True
        schemaDeclaresTimeout (object ["properties" .= object ["url" .= object []]])
          @?= False
        schemaDeclaresTimeout (object ["type" .= ("object" :: T.Text)])
          @?= False
    ]
