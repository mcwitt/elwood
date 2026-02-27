module Elwood.Tools
  ( -- * Types
    Tool (..),
    ToolResult (..),
    AgentContext (..),
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
  )
where

import Elwood.Tools.Attachment (mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types
