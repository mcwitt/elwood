module Elwood.Claude
  ( -- * Client
    ClaudeClient,
    newClient,
    sendMessages,
    sendMessagesWithRetry,
    RetryConfig (..),
    defaultRetryConfig,

    -- * Agent Loop
    AgentConfig (..),
    AgentResult (..),
    AgentObserver (..),
    RateLimitCallback,
    TextCallback,
    ToolUseCallback,
    runAgentTurn,

    -- * Conversation
    ConversationStore (..),
    newConversationStore,
    newInMemoryConversationStore,

    -- * Types
    CacheTtl (..),
    ClaudeMessage (..),
    ContentBlock (..),
    Conversation (..),
    Role (..),
    ToolName (..),
    ToolUseId (..),
    ToolSchema (..),
    MessagesRequest (..),
    MessagesResponse (..),
    Usage (..),
    StopReason (..),
    ClaudeError (..),
    ThinkingConfig (..),
    stopReasonToText,
  )
where

import Elwood.Claude.AgentLoop
import Elwood.Claude.Client
import Elwood.Claude.Conversation
import Elwood.Claude.Observer
import Elwood.Claude.Types
