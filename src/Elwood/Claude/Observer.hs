-- | Observer callbacks for agent loop metrics and telemetry.
-- Extracted from AgentLoop to break the dependency cycle where
-- Metrics imports AgentLoop just for the observer type.
module Elwood.Claude.Observer
  ( AgentObserver (..),
    RateLimitCallback,
    TextCallback,
    ToolUseCallback,
  )
where

import Data.Set (Set)
import Data.Text (Text)
import Elwood.Claude.Types
  ( ClaudeMessage,
    StopReason,
    ToolName,
    ToolSchema,
    Usage,
  )

-- | Callback for rate limit notifications
-- Arguments: retry attempt number, wait seconds
type RateLimitCallback = Int -> Int -> IO ()

-- | Callback for intermediate text content produced during tool-use turns
type TextCallback = Text -> IO ()

-- | Callback for tool use notifications (receives iteration number and list of tool names)
type ToolUseCallback = Int -> [Text] -> IO ()

-- | Observer callbacks for metrics and telemetry.
-- The agent loop fires these at key points without knowing the underlying
-- recording mechanism.
data AgentObserver = AgentObserver
  { -- | Estimated input token breakdown (system prompt, tool search, messages, schemas)
    onInputEstimate :: Maybe Text -> Maybe (Set ToolName) -> [ClaudeMessage] -> [ToolSchema] -> IO (),
    -- | API response received (stop reason, usage)
    onApiResponse :: StopReason -> Usage -> IO (),
    -- | Tool call about to be executed (tool name)
    onToolCall :: Text -> IO (),
    -- | Context compaction triggered
    onCompaction :: IO ()
  }
