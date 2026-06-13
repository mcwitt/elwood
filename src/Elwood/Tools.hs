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
    filterRegistry,
    applyToolFilter,

    -- * Tool Constructors
    mkRunCommandTool,
    mkQueueAttachmentTool,
    mkSaveMemoryTool,
    mkSearchMemoryTool,
    mkDelegateTaskTool,
    mkCheckTaskTool,
    mkAwaitTaskTool,
    mkCancelTaskTool,
    mkViewImageTool,
    mkScheduleCallbackTool,
    mkListCallbacksTool,
    mkCancelCallbackTool,

    -- * Async Task Store
    AsyncTaskStore,
    newAsyncTaskStore,
  )
where

import Elwood.Tools.AsyncTask (AsyncTaskStore, mkAwaitTaskTool, mkCancelTaskTool, mkCheckTaskTool, newAsyncTaskStore)
import Elwood.Tools.Attachment (mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Delegate (mkDelegateTaskTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Schedule (mkCancelCallbackTool, mkListCallbacksTool, mkScheduleCallbackTool)
import Elwood.Tools.Types
import Elwood.Tools.ViewImage (mkViewImageTool)
