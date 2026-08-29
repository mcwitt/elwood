module Elwood.MCP.Registry
  ( -- * Tool Discovery
    discoverTools,

    -- * Tool Conversion
    toTool,
    toolResultParts,

    -- * Server Management
    startMCPServers,

    -- * Schema normalization and augmentation
    normalizeInputSchema,
    extractTimeout,
    injectTimeoutProperty,
    schemaDeclaresTimeout,
    maxRequestTimeoutSeconds,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON (..), Result (..), Value (..), fromJSON, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Elwood.Claude.Types (ToolName (..), ToolResultPart (..), ToolSchema (..))
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.MCP.Client (defaultRequestTimeoutSeconds, sendRequest, spawnServer, stopServer)
import Elwood.MCP.Types
import Elwood.Tools.Registry (ToolRegistry, registerTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..))

-- | Maximum seconds the agent may request via the injected @timeout_seconds@
-- argument on an MCP tool call. Caps any single MCP request so a slow or hung
-- server can't block a turn indefinitely.
maxRequestTimeoutSeconds :: Int
maxRequestTimeoutSeconds = 300

-- | Argument key the agent uses to override the per-call MCP request timeout.
timeoutArgKey :: Key
timeoutArgKey = "timeout_seconds"

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
  result <- sendRequest server defaultRequestTimeoutSeconds "tools/list" Nothing
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

-- | Convert an MCP tool to an Elwood Tool. If the upstream schema already
-- owns a @timeout_seconds@ property, we leave it (and its value) alone;
-- otherwise we inject one and intercept it as a per-request timeout override.
-- Image-typed result content is converted to perceivable image parts.
toTool :: Text -> MCPServer -> MCPTool -> Tool
toTool serverName server mcpTool =
  let baseSchema = normalizeInputSchema mcpTool.inputSchema
      (schemaWithTimeout, extract)
        | schemaDeclaresTimeout mcpTool.inputSchema =
            (baseSchema, \v -> Right (defaultRequestTimeoutSeconds, v))
        | otherwise =
            (injectTimeoutProperty baseSchema, extractTimeout)
   in Tool
        { schema =
            ToolSchema
              { name = ToolName ("mcp_" <> serverName <> "_" <> mcpTool.name),
                description = fromMaybe "(MCP tool)" mcpTool.description,
                inputSchema = schemaWithTimeout
              },
          execute = executeMCPTool server mcpTool extract
        }

-- | Normalize an MCP input schema for the Anthropic tools API.
--
-- Anthropic requires an object schema and rejects @oneOf@, @allOf@, and
-- @anyOf@ directly under @input_schema@. Some MCP servers emit root unions
-- (for example, a discriminated union of actions), so merge their properties
-- into one object. Conflicting property schemas retain the alternatives in a
-- nested combinator, which Anthropic accepts. Local references are expanded
-- first because they may point into a root combinator that is removed.
normalizeInputSchema :: Value -> Value
normalizeInputSchema schema = normalizeResolved (resolveLocalRefs schema schema)
  where
    normalizeResolved (Object obj) =
      Object $
        KM.insert "type" (String "object") $
          foldl'
            (\acc (key, propertyCombinator, requiredMode) -> flattenCombinator key propertyCombinator requiredMode acc)
            obj
            [ ("allOf", "allOf", RequiredUnion),
              ("oneOf", "anyOf", RequiredIntersection),
              ("anyOf", "anyOf", RequiredIntersection)
            ]
    normalizeResolved _ = object ["type" .= ("object" :: Text)]

    flattenCombinator key propertyCombinator requiredMode obj =
      case KM.lookup key obj of
        Just (Array alternatives) ->
          let branchObjects = map asObject (V.toList alternatives)
              branchProperties = map propertiesOf branchObjects
              combinedProperties = foldl' (KM.unionWith (combineSchemas propertyCombinator)) KM.empty branchProperties
              rootProperties = propertiesOf obj
              properties = KM.unionWith (combineSchemas "allOf") rootProperties combinedProperties
              branchRequired = combineRequired requiredMode (map requiredOf branchObjects)
              required = Set.union (requiredOf obj) branchRequired
              withoutCombinator = KM.delete key obj
              withProperties
                | KM.null properties = withoutCombinator
                | otherwise = KM.insert "properties" (Object properties) withoutCombinator
              withRequired
                | Set.null required = KM.delete "required" withProperties
                | otherwise = KM.insert "required" (toJSON (Set.toList required)) withProperties
           in retainCommonAdditionalProperties obj branchObjects withRequired
        _ -> obj

    asObject value = case normalizeResolved value of
      Object obj -> obj
      _ -> KM.empty

-- | How required fields from combinator branches are combined. All fields
-- required by an @allOf@ branch remain required; a field from a union is only
-- unconditionally required when every branch requires it.
data RequiredMode = RequiredUnion | RequiredIntersection

combineRequired :: RequiredMode -> [Set.Set Text] -> Set.Set Text
combineRequired RequiredUnion required = Set.unions required
combineRequired RequiredIntersection [] = Set.empty
combineRequired RequiredIntersection (required : rest) = foldl' Set.intersection required rest

propertiesOf :: KM.KeyMap Value -> KM.KeyMap Value
propertiesOf obj = case KM.lookup "properties" obj of
  Just (Object properties) -> properties
  _ -> KM.empty

requiredOf :: KM.KeyMap Value -> Set.Set Text
requiredOf obj = case KM.lookup "required" obj of
  Just (Array required) -> Set.fromList [name | String name <- V.toList required]
  _ -> Set.empty

-- | Preserve @additionalProperties@ when every branch agrees. This is common
-- for generated discriminated unions and keeps the flattened schema strict.
retainCommonAdditionalProperties :: KM.KeyMap Value -> [KM.KeyMap Value] -> KM.KeyMap Value -> KM.KeyMap Value
retainCommonAdditionalProperties original branches result
  | KM.member "additionalProperties" original = result
  | otherwise = case traverse (KM.lookup "additionalProperties") branches of
      Just (value : values)
        | all (== value) values -> KM.insert "additionalProperties" value result
      _ -> result

combineSchemas :: Key -> Value -> Value -> Value
combineSchemas combinator left right
  | left == right = left
  | otherwise = object [combinator .= distinct (members left ++ members right)]
  where
    members (Object obj)
      | Just (Array values) <- KM.lookup combinator obj,
        KM.size obj == 1 =
          V.toList values
    members value = [value]

    distinct = foldl' (\values value -> if value `elem` values then values else values ++ [value]) []

-- | Expand local JSON references before root combinators are flattened.
resolveLocalRefs :: Value -> Value -> Value
resolveLocalRefs root = go Set.empty
  where
    go seen (Object obj) =
      case KM.lookup "$ref" obj of
        Just (String ref)
          | "#/" `T.isPrefixOf` ref,
            ref `Set.notMember` seen,
            Just target <- resolvePointer root ref ->
              let resolvedTarget = go (Set.insert ref seen) target
                  siblings = KM.map (go seen) (KM.delete "$ref" obj)
               in mergeRefSiblings resolvedTarget siblings
        _ -> Object (KM.map (go seen) obj)
    go seen (Array values) = Array (V.map (go seen) values)
    go _ value = value

mergeRefSiblings :: Value -> KM.KeyMap Value -> Value
mergeRefSiblings target siblings
  | KM.null siblings = target
mergeRefSiblings (Object target) siblings = Object (KM.union siblings target)
mergeRefSiblings target siblings = object ["allOf" .= [target, Object siblings]]

resolvePointer :: Value -> Text -> Maybe Value
resolvePointer root ref = foldl' step (Just root) segments
  where
    segments = map decodeSegment (T.splitOn "/" (T.drop 2 ref))
    decodeSegment = T.replace "~0" "~" . T.replace "~1" "/"

    step (Just (Object obj)) segment = KM.lookup (Key.fromText segment) obj
    step (Just (Array values)) segment = do
      index <- readMaybeInt segment
      values V.!? index
    step _ _ = Nothing

readMaybeInt :: Text -> Maybe Int
readMaybeInt text = case reads (T.unpack text) of
  [(value, "")] | value >= 0 -> Just value
  _ -> Nothing

-- | True if the upstream tool's input schema already declares a property
-- named @timeout_seconds@ — in which case we defer to the server's semantics
-- instead of intercepting the value.
schemaDeclaresTimeout :: Value -> Bool
schemaDeclaresTimeout (Object obj) = case KM.lookup "properties" obj of
  Just (Object props) -> KM.member timeoutArgKey props
  _ -> False
schemaDeclaresTimeout _ = False

-- | Add a @timeout_seconds@ property to the tool's input schema so the agent
-- can override the per-request MCP timeout.
injectTimeoutProperty :: Value -> Value
injectTimeoutProperty (Object obj) =
  let props = case KM.lookup "properties" obj of
        Just (Object p) -> p
        _ -> KM.empty
      props' = KM.insert timeoutArgKey timeoutSchema props
   in Object $ KM.insert "properties" (Object props') obj
injectTimeoutProperty v = v

-- | JSON Schema fragment describing the injected @timeout_seconds@ argument.
timeoutSchema :: Value
timeoutSchema =
  object
    [ "type" .= ("integer" :: Text),
      "minimum" .= (1 :: Int),
      "maximum" .= maxRequestTimeoutSeconds,
      "description"
        .= ( "Maximum seconds to wait for this MCP request before failing. \
             \Defaults to "
               <> T.pack (show defaultRequestTimeoutSeconds)
               <> "s. Use a larger value for slow upstreams."
           )
    ]

executeMCPTool ::
  MCPServer ->
  MCPTool ->
  (Value -> Either Text (Int, Value)) ->
  Value ->
  IO ToolResult
executeMCPTool server mcpTool extract input =
  case extract input of
    Left err -> pure $ ToolError err
    Right (timeoutSecs, args) -> do
      let params_ =
            object
              [ "name" .= mcpTool.name,
                "arguments" .= args
              ]

      result <-
        sendRequest server timeoutSecs "tools/call" (Just params_)
          `catch` \(e :: SomeException) ->
            pure $ Left $ MCPRequestError $ T.pack $ show e

      case result of
        Left (MCPToolError _code msg) -> pure $ ToolError msg
        Left err -> pure $ ToolError $ T.pack $ show err
        Right value -> pure $ ToolSuccess $ toolResultParts value

-- | Pull the optional @timeout_seconds@ argument out of a tool input object,
-- clamping it into [1, maxRequestTimeoutSeconds]. Returns the chosen timeout
-- and the remaining arguments to forward to the MCP server.
extractTimeout :: Value -> Either Text (Int, Value)
extractTimeout (Object obj) =
  case KM.lookup timeoutArgKey obj of
    Nothing -> Right (defaultRequestTimeoutSeconds, Object obj)
    Just (Number n) ->
      let clamped = max 1 (min maxRequestTimeoutSeconds (round n))
       in Right (clamped, Object (KM.delete timeoutArgKey obj))
    Just _ -> Left $ "Invalid '" <> Key.toText timeoutArgKey <> "' parameter (must be an integer)"
extractTimeout v = Right (defaultRequestTimeoutSeconds, v)

-- | Convert an MCP tool result into tool result parts. Image-typed content
-- becomes perceivable image parts (validated and resized centrally by the
-- agent loop); everything else is flattened to text. Adjacent text parts
-- are merged so text-only results keep their historical single-string
-- wire shape.
toolResultParts :: Value -> [ToolResultPart]
toolResultParts (Object obj) =
  case KM.lookup "content" obj of
    -- Canonicalize an empty content array to one empty text part (the
    -- historical "" result); an empty parts list would not round-trip
    -- through the string wire form.
    Just (Array arr) -> case mergeTextParts $ map contentPart (V.toList arr) of
      [] -> [ToolResultText ""]
      parts -> parts
    Just v -> [ToolResultText (renderValue v)]
    Nothing -> [ToolResultText (renderValue (Object obj))]
toolResultParts v = [ToolResultText (renderValue v)]

-- | Convert a single MCP content block to a tool result part
contentPart :: Value -> ToolResultPart
contentPart v@(Object obj)
  | Just (String "image") <- KM.lookup "type" obj,
    Just (String b64) <- KM.lookup "data" obj,
    Just (String mt) <- KM.lookup "mimeType" obj =
      ToolResultImage mt b64
  | otherwise = ToolResultText (extractContent v)
contentPart v = ToolResultText (extractContent v)

-- | Merge adjacent text parts, joining with newlines
mergeTextParts :: [ToolResultPart] -> [ToolResultPart]
mergeTextParts (ToolResultText a : ToolResultText b : rest) =
  mergeTextParts (ToolResultText (a <> "\n" <> b) : rest)
mergeTextParts (p : rest) = p : mergeTextParts rest
mergeTextParts [] = []

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
