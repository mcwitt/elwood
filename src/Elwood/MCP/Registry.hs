{-# LANGUAGE StrictData #-}

module Elwood.MCP.Registry
  ( -- * Tool Discovery
    discoverTools,

    -- * Tool Conversion
    toTool,

    -- * Server Management
    startMCPServers,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON (..), Result (..), Value (..), fromJSON, object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Elwood.Claude.Types (ToolName (..), ToolSchema (..))
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.MCP.Client (sendRequest, spawnServer, stopServer)
import Elwood.MCP.Types
import Elwood.Tools.Registry (ToolRegistry, registerTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..))

-- | Response from tools/list
newtype ToolsListResponse = ToolsListResponse
  { tools :: [MCPTool]
  }

instance FromJSON ToolsListResponse where
  parseJSON = withObject "ToolsListResponse" $ \v ->
    ToolsListResponse <$> v .: "tools"

-- | Query available tools from an MCP server
discoverTools :: MCPServer -> IO (Either MCPError [MCPTool])
discoverTools server = do
  result <- sendRequest server "tools/list" Nothing
  case result of
    Left err -> pure $ Left err
    Right value -> do
      case fromJSONValue value :: Maybe ToolsListResponse of
        Nothing -> pure $ Left $ MCPProtocolError "Failed to parse tools/list response"
        Just resp -> pure $ Right resp.tools

-- | Helper to parse JSON Value
fromJSONValue :: (FromJSON a) => Value -> Maybe a
fromJSONValue v = case fromJSON v of
  Error _ -> Nothing
  Success a -> Just a

-- | Convert an MCP tool to an Elwood Tool
toTool :: Text -> MCPServer -> MCPTool -> Tool
toTool serverName server mcpTool =
  Tool
    { schema =
        ToolSchema
          { name = ToolName ("mcp_" <> serverName <> "_" <> mcpTool.name),
            description = fromMaybe "(MCP tool)" mcpTool.description,
            inputSchema = ensureTypeObject mcpTool.inputSchema
          },
      execute = executeMCPTool server mcpTool
    }

-- | Ensure the input schema has type: "object" at the top level
ensureTypeObject :: Value -> Value
ensureTypeObject (Object obj) =
  Object $ KM.insert "type" (String "object") obj
ensureTypeObject v = v

-- | Execute an MCP tool
executeMCPTool :: MCPServer -> MCPTool -> Value -> IO ToolResult
executeMCPTool server mcpTool input = do
  let params_ =
        object
          [ "name" .= mcpTool.name,
            "arguments" .= input
          ]

  result <-
    sendRequest server "tools/call" (Just params_)
      `catch` \(e :: SomeException) ->
        pure $ Left $ MCPRequestError $ T.pack $ show e

  case result of
    Left (MCPToolError _code msg) -> pure $ ToolError msg
    Left err -> pure $ ToolError $ T.pack $ show err
    Right value -> pure $ ToolSuccess $ formatToolResult value

-- | Format tool result for display
formatToolResult :: Value -> Text
formatToolResult (Object obj) =
  case KM.lookup "content" obj of
    Just (Array arr) -> T.intercalate "\n" $ map extractContent (V.toList arr)
    Just v -> renderValue v
    Nothing -> renderValue (Object obj)
formatToolResult v = renderValue v

-- | Extract text content from MCP content blocks
extractContent :: Value -> Text
extractContent (Object obj) =
  case KM.lookup "text" obj of
    Just (String t) -> t
    _ -> renderValue (Object obj)
extractContent v = renderValue v

-- | Render a JSON value as text
renderValue :: Value -> Text
renderValue (String t) = t
renderValue (Number n) = T.pack $ show n
renderValue (Bool b) = if b then "true" else "false"
renderValue Null = "null"
renderValue v = T.pack $ show v

-- | Start all configured MCP servers and merge tools into registry
startMCPServers ::
  Logger ->
  [MCPServerConfig] ->
  ToolRegistry ->
  IO (ToolRegistry, [MCPServer])
startMCPServers logger configs registry = do
  -- Spawn each server, collecting successful ones
  results <- mapM (startOneServer logger) configs

  let (failures, successes) = partitionResults results
      servers = map fst successes
      allToolsList = concatMap snd successes

  -- Log any failures
  mapM_ (logServerFailure logger) failures

  -- Register all MCP tools
  let finalRegistry = foldr registerTool registry allToolsList

  logInfo
    logger
    "MCP initialization complete"
    [ ("servers", T.pack $ show $ length servers),
      ("tools", T.pack $ show $ length allToolsList)
    ]

  pure (finalRegistry, servers)

-- | Start a single MCP server and discover its tools
startOneServer ::
  Logger ->
  MCPServerConfig ->
  IO (Either (MCPServerConfig, MCPError) (MCPServer, [Tool]))
startOneServer logger cfg = do
  spawnResult <- spawnServer logger cfg
  case spawnResult of
    Left err -> pure $ Left (cfg, err)
    Right server -> do
      toolsResult <- discoverTools server
      case toolsResult of
        Left err -> do
          stopServer server
          pure $ Left (cfg, err)
        Right mcpTools -> do
          let ts = map (toTool cfg.name server) mcpTools
          logInfo
            logger
            "Discovered MCP tools"
            [ ("server", cfg.name),
              ("count", T.pack $ show $ length ts)
            ]
          pure $ Right (server, ts)

-- | Partition results into failures and successes
partitionResults ::
  [Either (MCPServerConfig, MCPError) (MCPServer, [Tool])] ->
  ([(MCPServerConfig, MCPError)], [(MCPServer, [Tool])])
partitionResults = foldr go ([], [])
  where
    go (Left err) (errs, succs) = (err : errs, succs)
    go (Right succ_) (errs, succs) = (errs, succ_ : succs)

-- | Log a server failure
logServerFailure :: Logger -> (MCPServerConfig, MCPError) -> IO ()
logServerFailure logger (cfg, err) =
  logWarn
    logger
    "MCP server failed to start"
    [ ("server", cfg.name),
      ("error", T.pack $ show err)
    ]
