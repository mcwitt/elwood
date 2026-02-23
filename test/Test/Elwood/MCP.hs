module Test.Elwood.MCP (tests) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Elwood.Config (MCPServerConfig (..))
import Elwood.MCP.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "MCP"
    [ jsonRpcTests,
      mcpToolTests,
      mcpErrorTests,
      mcpServerConfigTests
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
