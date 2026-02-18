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
                { jrqJsonrpc = "2.0",
                  jrqMethod = "tools/list",
                  jrqParams = Nothing,
                  jrqId = 1
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
        let params = object ["name" .= ("test" :: String)]
        let req =
              JsonRpcRequest
                { jrqJsonrpc = "2.0",
                  jrqMethod = "tools/call",
                  jrqParams = Just params,
                  jrqId = 42
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
            jrsJsonrpc resp @?= "2.0"
            jrsId resp @?= Just 1
            jrsError resp @?= Nothing
            case jrsResult resp of
              Just (Object _) -> pure ()
              _ -> assertFailure "Expected result object",
      testCase "JsonRpcResponse parses error" $ do
        let jsonStr =
              "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":1}"
        case eitherDecode jsonStr :: Either String JsonRpcResponse of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right resp -> do
            jrsResult resp @?= Nothing
            case jrsError resp of
              Just err -> do
                jreCode err @?= (-32600)
                jreMessage err @?= "Invalid Request"
              Nothing -> assertFailure "Expected error",
      testCase "JsonRpcError parses with data" $ do
        let jsonStr = "{\"code\":-32000,\"message\":\"Server error\",\"data\":{\"detail\":\"info\"}}"
        case eitherDecode jsonStr :: Either String JsonRpcError of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right rpcErr -> do
            jreCode rpcErr @?= (-32000)
            jreMessage rpcErr @?= "Server error"
            case jreData rpcErr of
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
            mtName tool @?= "read_file"
            mtDescription tool @?= Just "Read a file"
            case mtInputSchema tool of
              Object obj -> KM.member "properties" obj @?= True
              _ -> assertFailure "Expected schema object",
      testCase "parses tool without description" $ do
        let jsonStr = "{\"name\":\"simple_tool\",\"inputSchema\":{\"type\":\"object\"}}"
        case eitherDecode jsonStr :: Either String MCPTool of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right tool -> do
            mtName tool @?= "simple_tool"
            mtDescription tool @?= Nothing
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
                { mscName = "filesystem",
                  mscCommand = "npx",
                  mscArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
                  mscEnv = Just [("DEBUG", "true")],
                  mscStartupDelay = 2000
                }
        mscName config @?= "filesystem"
        mscCommand config @?= "npx"
        length (mscArgs config) @?= 3
        mscEnv config @?= Just [("DEBUG", "true")]
        mscStartupDelay config @?= 2000,
      testCase "can create config without env" $ do
        let config =
              MCPServerConfig
                { mscName = "simple",
                  mscCommand = "my-server",
                  mscArgs = [],
                  mscEnv = Nothing,
                  mscStartupDelay = 0
                }
        mscEnv config @?= Nothing
        mscStartupDelay config @?= 0
    ]
