{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Elwood.Claude.Types
  ( -- * Message Types
    Role (..)
  , ClaudeMessage (..)
  , Conversation (..)

    -- * Content Blocks
  , ContentBlock (..)

    -- * API Request/Response
  , MessagesRequest (..)
  , MessagesResponse (..)
  , ToolSchema (..)
  , Usage (..)

    -- * Errors
  , ClaudeError (..)
  ) where

import Control.Exception (Exception)
import Data.Aeson
import Data.Int (Int64)
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

-- | Content block in a message - can be text, tool use, or tool result
data ContentBlock
  = TextBlock Text
  | ToolUseBlock
      { tubId :: Text
      -- ^ Tool use ID (e.g., "toolu_...")
      , tubName :: Text
      -- ^ Tool name
      , tubInput :: Value
      -- ^ JSON arguments
      }
  | ToolResultBlock
      { trbToolUseId :: Text
      -- ^ ID of the tool use this is a result for
      , trbContent :: Text
      -- ^ Result content
      , trbIsError :: Bool
      -- ^ Whether this is an error result
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextBlock t) =
    object
      [ "type" .= ("text" :: Text)
      , "text" .= t
      ]
  toJSON (ToolUseBlock tid name input) =
    object
      [ "type" .= ("tool_use" :: Text)
      , "id" .= tid
      , "name" .= name
      , "input" .= input
      ]
  toJSON (ToolResultBlock tid content isErr) =
    object $
      [ "type" .= ("tool_result" :: Text)
      , "tool_use_id" .= tid
      , "content" .= content
      ]
        ++ ["is_error" .= True | isErr]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    blockType <- v .: "type"
    case blockType :: Text of
      "text" -> TextBlock <$> v .: "text"
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
      other -> fail $ "Unknown content block type: " <> show other

-- | A message in a Claude conversation
data ClaudeMessage = ClaudeMessage
  { cmRole :: Role
  , cmContent :: [ContentBlock]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ClaudeMessage where
  toJSON msg =
    object
      [ "role" .= cmRole msg
      , "content" .= cmContent msg
      ]

instance FromJSON ClaudeMessage where
  parseJSON = withObject "ClaudeMessage" $ \v ->
    ClaudeMessage
      <$> v .: "role"
      <*> v .: "content"

-- | A conversation with history
data Conversation = Conversation
  { convChatId :: Int64
  -- ^ Telegram chat ID
  , convMessages :: [ClaudeMessage]
  -- ^ Message history (oldest first)
  , convLastUpdated :: UTCTime
  -- ^ When conversation was last updated
  }
  deriving stock (Show, Generic)

instance ToJSON Conversation where
  toJSON conv =
    object
      [ "chatId" .= convChatId conv
      , "messages" .= convMessages conv
      , "lastUpdated" .= convLastUpdated conv
      ]

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \v ->
    Conversation
      <$> v .: "chatId"
      <*> v .: "messages"
      <*> v .: "lastUpdated"

-- | Tool schema for API requests
data ToolSchema = ToolSchema
  { tsName :: Text
  -- ^ Tool name
  , tsDescription :: Text
  -- ^ Tool description
  , tsInputSchema :: Value
  -- ^ JSON Schema for input parameters
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ToolSchema where
  toJSON ts =
    object
      [ "name" .= tsName ts
      , "description" .= tsDescription ts
      , "input_schema" .= tsInputSchema ts
      ]

-- | Request to the Claude Messages API
data MessagesRequest = MessagesRequest
  { mrModel :: Text
  -- ^ Model to use (e.g., "claude-sonnet-4-20250514")
  , mrMaxTokens :: Int
  -- ^ Maximum tokens in response
  , mrSystem :: Maybe Text
  -- ^ System prompt (optional)
  , mrMessages :: [ClaudeMessage]
  -- ^ Conversation messages
  , mrTools :: [ToolSchema]
  -- ^ Available tools
  }
  deriving stock (Show, Generic)

instance ToJSON MessagesRequest where
  toJSON req =
    object $
      [ "model" .= mrModel req
      , "max_tokens" .= mrMaxTokens req
      , "messages" .= mrMessages req
      ]
        ++ maybe [] (\s -> ["system" .= s]) (mrSystem req)
        ++ (if null (mrTools req) then [] else ["tools" .= mrTools req])

-- | Token usage information
data Usage = Usage
  { usageInputTokens :: Int
  , usageOutputTokens :: Int
  }
  deriving stock (Show, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "input_tokens"
      <*> v .: "output_tokens"

-- | Response from the Claude Messages API
data MessagesResponse = MessagesResponse
  { mresId :: Text
  -- ^ Unique message ID
  , mresContent :: [ContentBlock]
  -- ^ Response content blocks
  , mresStopReason :: Maybe Text
  -- ^ Why the model stopped (end_turn, max_tokens, tool_use, etc.)
  , mresUsage :: Usage
  -- ^ Token usage
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
  = ClaudeHttpError Int Text
  -- ^ HTTP error with status code and body
  | ClaudeParseError String
  -- ^ Failed to parse response JSON
  | ClaudeApiError Text Text
  -- ^ API error with type and message
  | ClaudeRateLimited
  -- ^ Rate limited by the API
  | ClaudeOverloaded
  -- ^ API is overloaded
  deriving stock (Show, Eq)

instance Exception ClaudeError
