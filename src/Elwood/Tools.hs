module Elwood.Tools
  ( -- * Types
    Tool (..),
    ToolResult (..),
    AgentContext (..),
    ApprovalOutcome (..),
    Attachment (..),
    AttachmentType (..),

    -- * Registry
    ToolRegistry,
    newToolRegistry,
    registerTool,
    lookupTool,
    allTools,
    toolSchemas,
    activeToolSchemas,

    -- * Tool Constructors
    mkRunCommandTool,
    mkQueueAttachmentTool,
    mkSaveMemoryTool,
    mkSearchMemoryTool,

    -- * Meta
    searchTools,
  )
where

import Elwood.Tools.Attachment (mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Meta (searchTools)
import Elwood.Tools.Registry
import Elwood.Tools.Types
