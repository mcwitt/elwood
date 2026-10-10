module Elwood.MCP.Registry (discoverTools, toTool, toolResultParts, startMCPServers, normalizeInputSchema, extractTimeout, injectTimeoutProperty, schemaDeclaresTimeout, maxRequestTimeoutSeconds) where

import Control.Exception (mask, onException)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Elwood.Claude.Types (ToolName (..), ToolResultPart (..), ToolSchema (..))
import Elwood.Config (MCPServerConfig (..))
import Elwood.Logging (Logger, logWarn)
import Elwood.MCP.Client
import Elwood.MCP.Types
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Registry (ToolRegistry, lookupTool, registerTool)
import Elwood.Tools.Types
import Text.Read (readMaybe)

maxRequestTimeoutSeconds :: Int
maxRequestTimeoutSeconds = 300

discoverTools :: MCPServer -> IO (Either MCPError [MCPTool])
discoverTools server = do
  result <- sendRequest server defaultRequestTimeoutSeconds "tools/list" Nothing
  pure $ result >>= either (Left . MCPProtocolError . T.pack) Right . parseEither (withObject "tools/list" (.: "tools"))

toTool :: Text -> MCPServer -> MCPTool -> Tool
toTool serverName server upstream = Tool (ToolSchema (ToolName ("mcp_" <> serverName <> "_" <> upstream.name)) (fromMaybe "MCP tool" upstream.description) exposed) $ \input ->
  case parseTimeout input of
    Left err -> pure (toolError err)
    Right (seconds, arguments) -> do
      response <- sendRequest server seconds "tools/call" (Just (object ["name" .= upstream.name, "arguments" .= arguments]))
      pure $ case response of
        Left err -> toolError (T.pack (show err))
        Right value@(Object result) | KM.lookup "isError" result == Just (Bool True) -> toolError (T.intercalate "\n" [text | ToolResultText text <- toolResultParts value])
        Right value -> ToolSuccess (toolResultParts value)
  where
    normalized = normalizeInputSchema upstream.inputSchema
    owned = schemaDeclaresTimeout normalized
    exposed = if owned then normalized else injectTimeoutProperty normalized
    parseTimeout = if owned then Right . (defaultRequestTimeoutSeconds,) else extractTimeout

schemaDeclaresTimeout :: Value -> Bool
schemaDeclaresTimeout = KM.member "timeout_seconds" . properties . asObject

injectTimeoutProperty :: Value -> Value
injectTimeoutProperty input = Object (KM.insert "properties" (Object (KM.insert "timeout_seconds" property (properties source))) source)
  where
    source = asObject input
    property = object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= maxRequestTimeoutSeconds, "description" .= ("Request timeout in seconds, default 30" :: Text)]

extractTimeout :: Value -> Either Text (Int, Value)
extractTimeout input@(Object source) = case KM.lookup "timeout_seconds" source of
  Nothing -> Right (defaultRequestTimeoutSeconds, input)
  Just value@(Number _) -> case B.parse (B.integer 1 maxRequestTimeoutSeconds) value of
    Left _ -> Left "timeout_seconds must be between 1 and 300"
    Right seconds -> Right (seconds, Object (KM.delete "timeout_seconds" source))
  Just _ -> Left "Invalid 'timeout_seconds' parameter (must be an integer)"
extractTimeout input = Right (defaultRequestTimeoutSeconds, input)

-- Root unions are an advertised approximation; the upstream server remains
-- responsible for validating branch-dependent constraints.
normalizeInputSchema :: Value -> Value
normalizeInputSchema input = normalize (resolve Set.empty input)
  where
    normalize = Object . KM.insert "type" (String "object") . (\source -> foldl' flatten source ["allOf", "anyOf", "oneOf"]) . asObject
    flatten source key = case KM.lookup key source of
      Just (Array alternatives) ->
        let branches = map (asObject . normalize) (V.toList alternatives)
            combination = if key == "allOf" then "allOf" else "anyOf"
            branchProperties = foldl' (KM.unionWith (combine combination)) KM.empty (map properties branches)
            fields = KM.unionWith (combine "allOf") (properties source) branchProperties
            required = Set.union (requiredKeys source) $ case map requiredKeys branches of
              [] -> Set.empty
              first : rest -> foldl' (if key == "allOf" then Set.union else Set.intersection) first rest
            merged = KM.insert "properties" (Object fields) (KM.insert "required" (toJSON (Set.toList required)) (KM.delete key source))
         in case traverse (KM.lookup "additionalProperties") branches of
              Just (first : rest) | not (KM.member "additionalProperties" source), all (== first) rest -> KM.insert "additionalProperties" first merged
              _ -> merged
      _ -> source
    resolve seen (Object source)
      | Just (String reference) <- KM.lookup "$ref" source,
        reference `Set.notMember` seen,
        Just target <- pointer reference =
          let resolved = resolve (Set.insert reference seen) target
              siblings = KM.map (resolve seen) (KM.delete "$ref" source)
           in if KM.null siblings then resolved else combine "allOf" resolved (Object siblings)
      | otherwise = Object (KM.map (resolve seen) source)
    resolve seen (Array values) = Array (V.map (resolve seen) values)
    resolve _ value = value
    pointer reference = do
      suffix <- T.stripPrefix "#/" reference
      foldl' step (Just input) (map (T.replace "~0" "~" . T.replace "~1" "/") (T.splitOn "/" suffix))
    step (Just (Object source)) key = KM.lookup (Key.fromText key) source
    step (Just (Array values)) key = readMaybe (T.unpack key) >>= (values V.!?)
    step _ _ = Nothing

asObject :: Value -> Object
asObject (Object source) = source
asObject _ = KM.empty

properties :: Object -> Object
properties source = maybe KM.empty asObject (KM.lookup "properties" source)

requiredKeys :: Object -> Set.Set Text
requiredKeys source = case KM.lookup "required" source of
  Just (Array values) -> Set.fromList [key | String key <- V.toList values]
  _ -> Set.empty

combine :: Key -> Value -> Value -> Value
combine key left right
  | left == right = left
  | otherwise = object [key .= nub (members left <> members right)]
  where
    members (Object source) | KM.size source == 1, Just (Array values) <- KM.lookup key source = V.toList values
    members value = [value]

toolResultParts :: Value -> [ToolResultPart]
toolResultParts (Object source) | Just (Array content) <- KM.lookup "content" source = case foldr (join . part) [] (V.toList content) of
  [] -> [ToolResultText ""]
  result -> result
  where
    part value@(Object block)
      | Just (String "image") <- KM.lookup "type" block,
        Just (String bytes) <- KM.lookup "data" block,
        Just (String mediaType) <- KM.lookup "mimeType" block =
          ToolResultImage mediaType bytes
      | otherwise = ToolResultText (render (fromMaybe value (KM.lookup "text" block)))
    part value = ToolResultText (render value)
    join (ToolResultText a) (ToolResultText b : rest) = ToolResultText (a <> "\n" <> b) : rest
    join value rest = value : rest
toolResultParts value = [ToolResultText (render value)]

render :: Value -> Text
render (String value) = value
render value = T.decodeUtf8 (BL.toStrict (encode value))

startMCPServers :: Logger -> [MCPServerConfig] -> ToolRegistry -> IO (ToolRegistry, [MCPServer])
startMCPServers logger configurations registry = mask $ \restore ->
  let start current opened [] = pure (current, reverse opened)
      start current opened (config : remaining) = do
        result <- restore (spawnServer logger config)
        case result of
          Left err -> report config err >> start current opened remaining
          Right server -> do
            discovery <- restore (discoverTools server) `onException` stopServer server
            case discovery of
              Left err -> stopServer server >> report config err >> start current opened remaining
              Right tools -> do
                let converted = map (toTool config.name server) tools
                    names = map (\tool -> tool.schema.name) converted
                    clashes = [name | name <- names, isJust (lookupTool name current)]
                if not (null clashes) || length names /= Set.size (Set.fromList names)
                  then stopServer server >> report config (MCPProtocolError "Duplicate MCP tool name") >> start current opened remaining
                  else start (foldr registerTool current converted) (server : opened) remaining `onException` stopServer server
      report config err = logWarn logger "MCP initialization failed" [("server", config.name), ("error", T.pack (show err))]
   in start registry [] configurations
