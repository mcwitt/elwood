{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Elwood.Claude.Types
  ( -- * Message Types
    Role (..),
    ClaudeMessage (..),
    Conversation (..),

    -- * Content Blocks
    ContentBlock (..),

    -- * Thinking
    ThinkingConfig (..),

    -- * Stop Reason
    StopReason (..),
    stopReasonToText,

    -- * API Request/Response
    MessagesRequest (..),
    MessagesResponse (..),
    ToolSchema (..),
    Usage (..),

    -- * Errors
    ClaudeError (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Elwood.Config (ThinkingEffort (..))
import GHC.Generics (Generic)

-- | Role in a conversation
data Role = User | Assistant
  deriving stock (Show, Eq, Generic)

instance ToJSON Role where
  toJSON User = "user"
  toJSON Assistant = "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    other -> fail $ "Unknown role: " <> show other

-- | Configuration for extended thinking
data ThinkingConfig
  = -- | Adaptive thinking with effort level
    ThinkingConfigAdaptive ThinkingEffort
  | -- | Fixed budget_tokens for older models
    ThinkingConfigBudget Int
  deriving stock (Show, Eq, Generic)

-- | Content block in a message - can be text, image, tool use, or tool result
data ContentBlock
  = TextBlock Text
  | ImageBlock
      { -- | Media type (e.g., "image/jpeg", "image/png")
        mediaType :: Text,
        -- | Base64-encoded image data
        data_ :: Text
      }
  | ToolUseBlock
      { -- | Tool use ID (e.g., "toolu_...")
        id_ :: Text,
        -- | Tool name
        name :: Text,
        -- | JSON arguments
        input :: Value
      }
  | ToolResultBlock
      { -- | ID of the tool use this is a result for
        toolUseId :: Text,
        -- | Result content
        content :: Text,
        -- | Whether this is an error result
        isError :: Bool
      }
  | ThinkingBlock
      { -- | The thinking text
        thinking :: Text,
        -- | Signature for verification
        signature :: Text
      }
  | RedactedThinkingBlock
      { -- | Opaque data for redacted thinking
        data_ :: Text
      }
  | -- | Server-side tool use (e.g., tool_search_tool_bm25) — preserved in history
    ServerToolUseBlock
      { id_ :: Text,
        name :: Text,
        input :: Value
      }
  | -- | Result from server-side tool search — preserved in history
    ToolSearchResultBlock
      { toolUseId :: Text,
        searchResult :: Value
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextBlock t) =
    object
      [ "type" .= ("text" :: Text),
        "text" .= t
      ]
  toJSON (ImageBlock mt imageData) =
    object
      [ "type" .= ("image" :: Text),
        "source"
          .= object
            [ "type" .= ("base64" :: Text),
              "media_type" .= mt,
              "data" .= imageData
            ]
      ]
  toJSON (ToolUseBlock tid n inp) =
    object
      [ "type" .= ("tool_use" :: Text),
        "id" .= tid,
        "name" .= n,
        "input" .= inp
      ]
  toJSON (ToolResultBlock tid c isErr) =
    object $
      [ "type" .= ("tool_result" :: Text),
        "tool_use_id" .= tid,
        "content" .= c
      ]
        ++ ["is_error" .= True | isErr]
  toJSON (ThinkingBlock t sig) =
    object
      [ "type" .= ("thinking" :: Text),
        "thinking" .= t,
        "signature" .= sig
      ]
  toJSON (RedactedThinkingBlock d) =
    object
      [ "type" .= ("redacted_thinking" :: Text),
        "data" .= d
      ]
  toJSON (ServerToolUseBlock tid n inp) =
    object
      [ "type" .= ("server_tool_use" :: Text),
        "id" .= tid,
        "name" .= n,
        "input" .= inp
      ]
  toJSON (ToolSearchResultBlock tid sr) =
    object
      [ "type" .= ("tool_search_tool_result" :: Text),
        "tool_use_id" .= tid,
        "content" .= sr
      ]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    blockType <- v .: "type"
    case blockType :: Text of
      "text" -> TextBlock <$> v .: "text"
      "image" -> do
        source <- v .: "source"
        ImageBlock
          <$> source .: "media_type"
          <*> source .: "data"
      "tool_use" ->
        ToolUseBlock
          <$> v .: "id"
          <*> v .: "name"
          <*> v .: "input"
      "tool_result" ->
        ToolResultBlock
          <$> v .: "tool_use_id"
          <*> v .: "content"
          <*> v .:? "is_error" .!= False
      "thinking" ->
        ThinkingBlock
          <$> v .: "thinking"
          <*> v .: "signature"
      "redacted_thinking" ->
        RedactedThinkingBlock
          <$> v .: "data"
      "server_tool_use" ->
        ServerToolUseBlock
          <$> v .: "id"
          <*> v .: "name"
          <*> v .: "input"
      "tool_search_tool_result" ->
        ToolSearchResultBlock
          <$> v .: "tool_use_id"
          <*> v .: "content"
      other -> fail $ "Unknown content block type: " <> show other

-- | A message in a Claude conversation
data ClaudeMessage = ClaudeMessage
  { role :: Role,
    content :: [ContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ClaudeMessage where
  toJSON msg =
    object
      [ "role" .= msg.role,
        "content" .= msg.content
      ]

instance FromJSON ClaudeMessage where
  parseJSON = withObject "ClaudeMessage" $ \v ->
    ClaudeMessage
      <$> v .: "role"
      <*> v .: "content"

-- | A conversation with history
data Conversation = Conversation
  { -- | Session identifier (e.g. chat ID or named session)
    sessionId :: Text,
    -- | Message history (oldest first)
    messages :: [ClaudeMessage],
    -- | When conversation was last updated
    lastUpdated :: UTCTime
  }
  deriving stock (Show, Generic)

instance ToJSON Conversation where
  toJSON conv =
    object
      [ "sessionId" .= conv.sessionId,
        "messages" .= conv.messages,
        "lastUpdated" .= conv.lastUpdated
      ]

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \v ->
    Conversation
      <$> v .: "sessionId"
      <*> v .: "messages"
      <*> v .: "lastUpdated"

-- | Why the model stopped generating
data StopReason
  = -- | Normal completion
    EndTurn
  | -- | Model wants to use a tool
    ToolUse
  | -- | Hit the max_tokens limit
    MaxTokens
  | -- | Unknown or new stop reason
    StopReasonOther Text
  deriving stock (Show, Eq, Generic)

-- | Convert a stop reason to its API text representation
stopReasonToText :: StopReason -> Text
stopReasonToText EndTurn = "end_turn"
stopReasonToText ToolUse = "tool_use"
stopReasonToText MaxTokens = "max_tokens"
stopReasonToText (StopReasonOther t) = t

-- | Parse a stop reason from API text
parseStopReason :: Text -> StopReason
parseStopReason "end_turn" = EndTurn
parseStopReason "tool_use" = ToolUse
parseStopReason "max_tokens" = MaxTokens
parseStopReason other = StopReasonOther other

-- | Tool schema for API requests
data ToolSchema = ToolSchema
  { -- | Tool name
    name :: Text,
    -- | Tool description
    description :: Text,
    -- | JSON Schema for input parameters
    inputSchema :: Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ToolSchema where
  toJSON ts =
    object
      [ "name" .= ts.name,
        "description" .= ts.description,
        "input_schema" .= ts.inputSchema
      ]

-- | Request to the Claude Messages API
data MessagesRequest = MessagesRequest
  { -- | Model to use (e.g., "claude-sonnet-4-20250514")
    model :: Text,
    -- | Maximum tokens in response
    maxTokens :: Int,
    -- | System prompt (optional)
    system :: Maybe Text,
    -- | Conversation messages
    messages :: [ClaudeMessage],
    -- | Available tools
    tools :: [ToolSchema],
    -- | Extended thinking configuration (optional)
    thinking :: Maybe ThinkingConfig,
    -- | Enable automatic prompt caching
    cacheControl :: Bool,
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set Text)
  }
  deriving stock (Show, Generic)

instance ToJSON MessagesRequest where
  toJSON req =
    object $
      [ "model" .= req.model,
        "max_tokens" .= req.maxTokens,
        "messages" .= req.messages
      ]
        ++ maybe [] (\s -> ["system" .= s]) req.system
        ++ toolsField
        ++ maybe [] thinkingFields req.thinking
        ++ ["cache_control" .= object ["type" .= ("ephemeral" :: Text)] | req.cacheControl]
    where
      toolsField :: [Pair]
      toolsField
        | null req.tools = []
        | otherwise = case req.toolSearch of
            Nothing ->
              ["tools" .= req.tools]
            Just neverDefer ->
              let searchTool =
                    object
                      [ "type" .= ("tool_search_tool_bm25_20251119" :: Text),
                        "name" .= ("tool_search_tool_bm25" :: Text)
                      ]
                  addDefer ts =
                    object
                      [ "name" .= ts.name,
                        "description" .= ts.description,
                        "input_schema" .= ts.inputSchema,
                        "defer_loading" .= not (Set.member ts.name neverDefer)
                      ]
                  toolValues = map addDefer req.tools
               in ["tools" .= (searchTool : toolValues)]

      effortToText :: ThinkingEffort -> Text
      effortToText EffortLow = "low"
      effortToText EffortMedium = "medium"
      effortToText EffortHigh = "high"

      thinkingFields :: ThinkingConfig -> [Pair]
      thinkingFields (ThinkingConfigAdaptive effort) =
        [ "thinking" .= object ["type" .= ("adaptive" :: Text)],
          "output_config" .= object ["effort" .= effortToText effort]
        ]
      thinkingFields (ThinkingConfigBudget n) =
        [ "thinking"
            .= object
              [ "type" .= ("enabled" :: Text),
                "budget_tokens" .= n
              ]
        ]

-- | Token usage information
data Usage = Usage
  { inputTokens :: Int,
    outputTokens :: Int,
    cacheCreationInputTokens :: Int,
    cacheReadInputTokens :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "input_tokens"
      <*> v .: "output_tokens"
      <*> v .:? "cache_creation_input_tokens" .!= 0
      <*> v .:? "cache_read_input_tokens" .!= 0

-- | Response from the Claude Messages API
data MessagesResponse = MessagesResponse
  { -- | Unique message ID
    id_ :: Text,
    -- | Response content blocks
    content :: [ContentBlock],
    -- | Why the model stopped
    stopReason :: StopReason,
    -- | Token usage
    usage :: Usage
  }
  deriving stock (Show, Generic)

instance FromJSON MessagesResponse where
  parseJSON = withObject "MessagesResponse" $ \v ->
    MessagesResponse
      <$> v .: "id"
      <*> v .: "content"
      <*> (maybe (StopReasonOther "null") parseStopReason <$> v .:? "stop_reason")
      <*> v .: "usage"

-- | Errors from Claude API
data ClaudeError
  = -- | HTTP error with status code and body
    ClaudeHttpError Int Text
  | -- | Failed to parse response JSON
    ClaudeParseError String
  | -- | API error with type and message
    ClaudeApiError Text Text
  | -- | Rate limited by the API (with optional retry-after seconds)
    ClaudeRateLimited (Maybe Int)
  | -- | API is overloaded (with optional retry-after seconds)
    ClaudeOverloaded (Maybe Int)
  deriving stock (Show, Eq)

instance Exception ClaudeError
