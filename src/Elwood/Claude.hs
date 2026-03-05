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

    -- * Compaction
    compactIfNeeded,
    compactMessages,

    -- * Types
    CacheTtl (..),
    cacheTtlSeconds,
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
import Elwood.Claude.Compaction (compactIfNeeded, compactMessages)
import Elwood.Claude.Conversation
import Elwood.Claude.Observer
import Elwood.Claude.Types
