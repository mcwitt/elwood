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
import Data.Text (Text)
import Data.Time (UTCTime)
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
newtype ThinkingConfig = ThinkingConfig
  { tcBudgetTokens :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Content block in a message - can be text, image, tool use, or tool result
data ContentBlock
  = TextBlock Text
  | ImageBlock
      { -- | Media type (e.g., "image/jpeg", "image/png")
        ibMediaType :: Text,
        -- | Base64-encoded image data
        ibData :: Text
      }
  | ToolUseBlock
      { -- | Tool use ID (e.g., "toolu_...")
        tubId :: Text,
        -- | Tool name
        tubName :: Text,
        -- | JSON arguments
        tubInput :: Value
      }
  | ToolResultBlock
      { -- | ID of the tool use this is a result for
        trbToolUseId :: Text,
        -- | Result content
        trbContent :: Text,
        -- | Whether this is an error result
        trbIsError :: Bool
      }
  | ThinkingBlock
      { -- | The thinking text
        tbThinking :: Text,
        -- | Signature for verification
        tbSignature :: Text
      }
  | RedactedThinkingBlock
      { -- | Opaque data for redacted thinking
        rtbData :: Text
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextBlock t) =
    object
      [ "type" .= ("text" :: Text),
        "text" .= t
      ]
  toJSON (ImageBlock mediaType imageData) =
    object
      [ "type" .= ("image" :: Text),
        "source"
          .= object
            [ "type" .= ("base64" :: Text),
              "media_type" .= mediaType,
              "data" .= imageData
            ]
      ]
  toJSON (ToolUseBlock tid name input) =
    object
      [ "type" .= ("tool_use" :: Text),
        "id" .= tid,
        "name" .= name,
        "input" .= input
      ]
  toJSON (ToolResultBlock tid content isErr) =
    object $
      [ "type" .= ("tool_result" :: Text),
        "tool_use_id" .= tid,
        "content" .= content
      ]
        ++ ["is_error" .= True | isErr]
  toJSON (ThinkingBlock thinking sig) =
    object
      [ "type" .= ("thinking" :: Text),
        "thinking" .= thinking,
        "signature" .= sig
      ]
  toJSON (RedactedThinkingBlock d) =
    object
      [ "type" .= ("redacted_thinking" :: Text),
        "data" .= d
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
      other -> fail $ "Unknown content block type: " <> show other

-- | A message in a Claude conversation
data ClaudeMessage = ClaudeMessage
  { cmRole :: Role,
    cmContent :: [ContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ClaudeMessage where
  toJSON msg =
    object
      [ "role" .= cmRole msg,
        "content" .= cmContent msg
      ]

instance FromJSON ClaudeMessage where
  parseJSON = withObject "ClaudeMessage" $ \v ->
    ClaudeMessage
      <$> v .: "role"
      <*> v .: "content"

-- | A conversation with history
data Conversation = Conversation
  { -- | Session identifier (e.g. chat ID or named session)
    convSessionId :: Text,
    -- | Message history (oldest first)
    convMessages :: [ClaudeMessage],
    -- | When conversation was last updated
    convLastUpdated :: UTCTime
  }
  deriving stock (Show, Generic)

instance ToJSON Conversation where
  toJSON conv =
    object
      [ "sessionId" .= convSessionId conv,
        "messages" .= convMessages conv,
        "lastUpdated" .= convLastUpdated conv
      ]

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \v ->
    Conversation
      <$> v .: "sessionId"
      <*> v .: "messages"
      <*> v .: "lastUpdated"

-- | Tool schema for API requests
data ToolSchema = ToolSchema
  { -- | Tool name
    tsName :: Text,
    -- | Tool description
    tsDescription :: Text,
    -- | JSON Schema for input parameters
    tsInputSchema :: Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ToolSchema where
  toJSON ts =
    object
      [ "name" .= tsName ts,
        "description" .= tsDescription ts,
        "input_schema" .= tsInputSchema ts
      ]

-- | Request to the Claude Messages API
data MessagesRequest = MessagesRequest
  { -- | Model to use (e.g., "claude-sonnet-4-20250514")
    mrModel :: Text,
    -- | Maximum tokens in response
    mrMaxTokens :: Int,
    -- | System prompt (optional)
    mrSystem :: Maybe Text,
    -- | Conversation messages
    mrMessages :: [ClaudeMessage],
    -- | Available tools
    mrTools :: [ToolSchema],
    -- | Extended thinking configuration (optional)
    mrThinking :: Maybe ThinkingConfig
  }
  deriving stock (Show, Generic)

instance ToJSON MessagesRequest where
  toJSON req =
    object $
      [ "model" .= mrModel req,
        "max_tokens" .= mrMaxTokens req,
        "messages" .= mrMessages req
      ]
        ++ maybe [] (\s -> ["system" .= s]) (mrSystem req)
        ++ ["tools" .= mrTools req | not (null (mrTools req))]
        ++ maybe
          []
          ( \tc ->
              [ "thinking"
                  .= object
                    [ "type" .= ("enabled" :: Text),
                      "budget_tokens" .= tcBudgetTokens tc
                    ]
              ]
          )
          (mrThinking req)

-- | Token usage information
data Usage = Usage
  { usageInputTokens :: Int,
    usageOutputTokens :: Int,
    usageCacheCreationInputTokens :: Int,
    usageCacheReadInputTokens :: Int
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
    mresId :: Text,
    -- | Response content blocks
    mresContent :: [ContentBlock],
    -- | Why the model stopped (end_turn, max_tokens, tool_use, etc.)
    mresStopReason :: Maybe Text,
    -- | Token usage
    mresUsage :: Usage
  }
  deriving stock (Show, Generic)

instance FromJSON MessagesResponse where
  parseJSON = withObject "MessagesResponse" $ \v ->
    MessagesResponse
      <$> v .: "id"
      <*> v .: "content"
      <*> v .:? "stop_reason"
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
