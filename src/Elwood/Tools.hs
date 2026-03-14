module Elwood.Tools
  ( -- * Types
    Tool (..),
    ToolResult (..),
    ApprovalFunction,
    ApprovalOutcome (..),
    noApprovalChannel,
    Attachment (..),
    AttachmentType (..),

    -- * Registry
    ToolRegistry,
    newToolRegistry,
    registerTool,
    lookupTool,
    allTools,
    toolSchemas,

    -- * Tool Constructors
    mkRunCommandTool,
    mkQueueAttachmentTool,
    mkSaveMemoryTool,
    mkSearchMemoryTool,
    mkDelegateTaskTool,
    mkCheckTaskTool,
    mkStopTaskTool,

    -- * Async Task Store
    AsyncTaskStore,
    newAsyncTaskStore,
  )
where

import Elwood.Tools.AsyncTask (AsyncTaskStore, mkCheckTaskTool, mkStopTaskTool, newAsyncTaskStore)
import Elwood.Tools.Attachment (mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Delegate (mkDelegateTaskTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types
