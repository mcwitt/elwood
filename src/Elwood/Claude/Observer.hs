module Elwood.Claude.Observer (AgentObserver (..), RateLimitCallback, TextCallback, ToolUseCallback) where

import Data.Aeson (Value)
import Data.Set (Set)
import Data.Text (Text)
import Elwood.Claude.Types (ClaudeMessage, StopReason, ToolName, ToolSchema, Usage)

type RateLimitCallback = Int -> Int -> IO ()

type TextCallback = Text -> IO ()

type ToolUseCallback = Int -> [(Text, Value)] -> IO ()

-- | Observations compose in the same order as the interpreted turn trace.
data AgentObserver = AgentObserver
  { onInputEstimate :: Maybe Text -> Maybe (Set ToolName) -> [ClaudeMessage] -> [ToolSchema] -> IO (),
    onApiResponse :: StopReason -> Usage -> IO (),
    onToolCall :: Text -> IO (),
    onCompaction :: IO ()
  }
