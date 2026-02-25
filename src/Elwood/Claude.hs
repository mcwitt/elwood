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
    RateLimitCallback,
    TextCallback,
    runAgentTurn,

    -- * Conversation
    ConversationStore,
    newConversationStore,
    getConversation,
    allConversations,
    updateConversation,
    clearConversation,

    -- * Compaction
    compactIfNeeded,

    -- * Types
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
import Elwood.Claude.Compaction (compactIfNeeded)
import Elwood.Claude.Conversation
import Elwood.Claude.Types
