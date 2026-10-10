module Elwood.Claude.Types
  ( Role (..),
    ClaudeMessage (..),
    Conversation (..),
    turnBoundaryIndices,
    ContentBlock (..),
    ToolResultPart (..),
    toolResultText,
    ToolName (..),
    ToolUseId (..),
    ThinkingConfig (..),
    StopReason (..),
    stopReasonToText,
    CacheTtl (..),
    cacheTtlSeconds,
    epoch,
    extendCacheExpiry,
    OutputFormat (..),
    jsonSchemaFormat,
    MessagesRequest (..),
    MessagesResponse (..),
    ToolSchema (..),
    Usage (..),
    ClaudeError (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
import Elwood.Thinking (ThinkingDisplay (..), ThinkingEffort (..))

newtype ToolName = ToolName {unToolName :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, IsString)

newtype ToolUseId = ToolUseId {unToolUseId :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

data Role = User | Assistant deriving stock (Show, Eq)

instance ToJSON Role where
  toJSON =
    String . \case
      User -> "user"
      Assistant -> "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    other -> fail ("Unknown role: " <> show other)

data ThinkingConfig = ThinkingConfigAdaptive (Maybe ThinkingEffort) (Maybe ThinkingDisplay) | ThinkingConfigBudget Int
  deriving stock (Show, Eq)

data ToolResultPart = ToolResultText Text | ToolResultImage Text Text deriving stock (Show, Eq)

data ContentBlock
  = TextBlock Text
  | ImageBlock Text Text
  | ToolUseBlock ToolUseId ToolName Value
  | ToolResultBlock ToolUseId [ToolResultPart] Bool
  | ThinkingBlock Text Text
  | RedactedThinkingBlock Text
  | ServerToolUseBlock ToolUseId ToolName Value
  | ToolSearchResultBlock ToolUseId Value
  | OpaqueBlock Object
  deriving stock (Show, Eq)

toolResultText :: [ToolResultPart] -> Text
toolResultText parts = T.intercalate "\n" [text | ToolResultText text <- parts]

-- Wire tags are explicit; unknown objects survive exactly as JSON objects.
blockObject :: Text -> [Pair] -> Value
blockObject tag fields = object ("type" .= tag : fields)

imageObject :: Text -> Text -> Value
imageObject media bytes = blockObject "image" ["source" .= blockObject "base64" ["media_type" .= media, "data" .= bytes]]

parseImage :: (Text -> Text -> a) -> Object -> Parser a
parseImage make object_ = do
  source <- object_ .: "source"
  kind <- source .: "type"
  if kind /= ("base64" :: Text) then fail "image source must be base64" else make <$> source .: "media_type" <*> source .: "data"

instance ToJSON ToolResultPart where
  toJSON (ToolResultText text) = blockObject "text" ["text" .= text]
  toJSON (ToolResultImage media bytes) = imageObject media bytes

instance FromJSON ToolResultPart where
  parseJSON = withObject "ToolResultPart" $ \v ->
    v .: "type" >>= \case
      ("text" :: Text) -> ToolResultText <$> v .: "text"
      "image" -> parseImage ToolResultImage v
      other -> fail ("Unknown tool result part type: " <> show other)

instance ToJSON ContentBlock where
  toJSON = \case
    TextBlock text -> toJSON (ToolResultText text)
    ImageBlock media bytes -> imageObject media bytes
    ToolUseBlock ident name input -> call "tool_use" ident name input
    ServerToolUseBlock ident name input -> call "server_tool_use" ident name input
    ToolResultBlock ident parts isError ->
      blockObject
        "tool_result"
        (["tool_use_id" .= ident, "content" .= resultContent parts] <> ["is_error" .= True | isError])
    ThinkingBlock text signature -> blockObject "thinking" ["thinking" .= text, "signature" .= signature]
    RedactedThinkingBlock bytes -> blockObject "redacted_thinking" ["data" .= bytes]
    ToolSearchResultBlock ident content -> blockObject "tool_search_tool_result" ["tool_use_id" .= ident, "content" .= content]
    OpaqueBlock value -> Object value
    where
      call tag ident name input = blockObject tag ["id" .= ident, "name" .= name, "input" .= input]
      resultContent parts
        | all isText parts = String (toolResultText parts)
        | otherwise = toJSON parts
      isText (ToolResultText _) = True
      isText _ = False

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v ->
    v .: "type" >>= \case
      ("text" :: Text) -> TextBlock <$> v .: "text"
      "image" -> parseImage ImageBlock v
      "tool_use" -> ToolUseBlock <$> v .: "id" <*> v .: "name" <*> v .: "input"
      "server_tool_use" -> ServerToolUseBlock <$> v .: "id" <*> v .: "name" <*> v .: "input"
      "tool_result" -> do
        content <- v .: "content"
        parts <- case content of
          String text -> pure [ToolResultText text]
          Array _ -> parseJSON content
          _ -> fail "tool_result content must be a string or array"
        ToolResultBlock <$> v .: "tool_use_id" <*> pure parts <*> v .:? "is_error" .!= False
      "thinking" -> ThinkingBlock <$> v .: "thinking" <*> v .: "signature"
      "redacted_thinking" -> RedactedThinkingBlock <$> v .: "data"
      "tool_search_tool_result" -> ToolSearchResultBlock <$> v .: "tool_use_id" <*> v .: "content"
      _ -> pure (OpaqueBlock v)

data ClaudeMessage = ClaudeMessage {role :: Role, content :: [ContentBlock]} deriving stock (Show, Eq)

instance ToJSON ClaudeMessage where
  toJSON message = object ["role" .= message.role, "content" .= message.content]

instance FromJSON ClaudeMessage where
  parseJSON = withObject "Message" $ \v -> ClaudeMessage <$> v .: "role" <*> v .: "content"

data Conversation = Conversation {sessionId :: Text, messages :: [ClaudeMessage], cacheExpiresAt :: UTCTime} deriving stock (Show)

instance ToJSON Conversation where
  toJSON conversation = object ["sessionId" .= conversation.sessionId, "messages" .= conversation.messages, "cacheExpiresAt" .= conversation.cacheExpiresAt]

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \v -> Conversation <$> v .: "sessionId" <*> v .: "messages" <*> v .:? "cacheExpiresAt" .!= epoch

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

extendCacheExpiry :: UTCTime -> CacheTtl -> UTCTime -> UTCTime
extendCacheExpiry now ttl = max (addUTCTime (cacheTtlSeconds ttl) now)

data StopReason = EndTurn | ToolUse | MaxTokens | StopReasonOther Text deriving stock (Show, Eq)

stopReasonToText :: StopReason -> Text
stopReasonToText = \case
  EndTurn -> "end_turn"
  ToolUse -> "tool_use"
  MaxTokens -> "max_tokens"
  StopReasonOther text -> text

parseStop :: Text -> StopReason
parseStop = \case
  "end_turn" -> EndTurn
  "tool_use" -> ToolUse
  "max_tokens" -> MaxTokens
  other -> StopReasonOther other

data ToolSchema = ToolSchema {name :: ToolName, description :: Text, inputSchema :: Value} deriving stock (Show, Eq)

instance ToJSON ToolSchema where
  toJSON schema = object ["name" .= schema.name, "description" .= schema.description, "input_schema" .= schema.inputSchema]

data CacheTtl = CacheTtl5Min | CacheTtl1Hour deriving stock (Show, Eq, Ord)

cacheTtlSeconds :: CacheTtl -> NominalDiffTime
cacheTtlSeconds CacheTtl5Min = 300
cacheTtlSeconds CacheTtl1Hour = 3600

instance FromJSON CacheTtl where
  parseJSON = withText "CacheTtl" $ \case
    "5m" -> pure CacheTtl5Min
    "1h" -> pure CacheTtl1Hour
    other -> fail ("Invalid cache_ttl: " <> show other)

newtype OutputFormat = OutputFormat {unOutputFormat :: Value}
  deriving stock (Show)
  deriving newtype (ToJSON)

jsonSchemaFormat :: Value -> OutputFormat
jsonSchemaFormat schema = OutputFormat (blockObject "json_schema" ["schema" .= schema])

data MessagesRequest = MessagesRequest
  { model :: Text,
    maxTokens :: Int,
    system :: Maybe Text,
    messages :: [ClaudeMessage],
    tools :: [ToolSchema],
    thinking :: Maybe ThinkingConfig,
    cacheControl :: Maybe CacheTtl,
    toolSearch :: Maybe (Set ToolName),
    outputFormat :: Maybe OutputFormat
  }
  deriving stock (Show)

instance ToJSON MessagesRequest where
  toJSON req =
    object $
      ["model" .= req.model, "max_tokens" .= req.maxTokens, "messages" .= req.messages]
        <> ["system" .= systemValue text | Just text <- [req.system]]
        <> ["tools" .= toolValues | not (null req.tools)]
        <> ["cache_control" .= cache ttl | Just ttl <- [req.cacheControl]]
        <> ["thinking" .= value | Just value <- [thinkingValue req.thinking]]
        <> ["output_config" .= object output | not (null output)]
    where
      cache ttl = blockObject "ephemeral" ["ttl" .= ("1h" :: Text) | ttl == CacheTtl1Hour]
      systemValue text = case req.cacheControl of
        Nothing -> String text
        Just ttl -> toJSON [blockObject "text" ["text" .= text, "cache_control" .= cache ttl]]
      toolValues = case req.toolSearch of
        Nothing -> map toJSON req.tools
        Just immediate ->
          blockObject "tool_search_tool_bm25_20251119" ["name" .= ("tool_search_tool_bm25" :: Text)]
            : [object ["name" .= tool.name, "description" .= tool.description, "input_schema" .= tool.inputSchema, "defer_loading" .= Set.notMember tool.name immediate] | tool <- req.tools]
      thinkingValue Nothing = Nothing
      thinkingValue (Just (ThinkingConfigBudget tokens)) = Just (blockObject "enabled" ["budget_tokens" .= tokens])
      thinkingValue (Just (ThinkingConfigAdaptive _ display)) = Just (blockObject "adaptive" ["display" .= displayText d | Just d <- [display]])
      output =
        ["format" .= format | Just format <- [req.outputFormat]]
          <> ["effort" .= effortText effort | Just (ThinkingConfigAdaptive (Just effort) _) <- [req.thinking]]
      displayText = \case
        DisplayOmitted -> "omitted" :: Text
        DisplaySummarized -> "summarized"
        DisplayUpdates -> "updates"
      effortText = \case
        EffortLow -> "low" :: Text
        EffortMedium -> "medium"
        EffortHigh -> "high"
        EffortXhigh -> "xhigh"
        EffortMax -> "max"

data Usage = Usage
  { inputTokens :: Int,
    outputTokens :: Int,
    cacheCreationInputTokens :: Int,
    cacheReadInputTokens :: Int,
    cacheCreation5mTokens :: Int,
    cacheCreation1hTokens :: Int
  }
  deriving stock (Show, Eq)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v -> do
    created <- v .:? "cache_creation_input_tokens" .!= 0
    split <- v .:? "cache_creation"
    (short, long) <- maybe (pure (created, 0)) (\s -> (,) <$> s .:? "ephemeral_5m_input_tokens" .!= 0 <*> s .:? "ephemeral_1h_input_tokens" .!= 0) split
    Usage <$> v .: "input_tokens" <*> v .: "output_tokens" <*> pure created <*> v .:? "cache_read_input_tokens" .!= 0 <*> pure short <*> pure long

data MessagesResponse = MessagesResponse {id_ :: Text, content :: [ContentBlock], stopReason :: StopReason, usage :: Usage} deriving stock (Show)

instance FromJSON MessagesResponse where
  parseJSON = withObject "MessagesResponse" $ \v -> MessagesResponse <$> v .: "id" <*> v .: "content" <*> (maybe (StopReasonOther "null") parseStop <$> v .:? "stop_reason") <*> v .: "usage"

data ClaudeError
  = ClaudeHttpError Int Text
  | ClaudeParseError String
  | ClaudeApiError Text Text
  | ClaudeRateLimited (Maybe Int)
  | ClaudeOverloaded (Maybe Int)
  | ClaudeUnknownProvider Text
  deriving stock (Show, Eq)

instance Exception ClaudeError

-- Stored history is canonical: image hoisting is restricted to HTTP projection.
turnBoundaryIndices :: [ClaudeMessage] -> [Int]
turnBoundaryIndices messages_ = [index | (index, ClaudeMessage User blocks) <- zip [0 ..] messages_, any input blocks]
  where
    input (ToolResultBlock {}) = False
    input _ = True
