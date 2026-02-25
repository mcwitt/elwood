{-# LANGUAGE StrictData #-}

module Elwood.Tools.Registry
  ( -- * Tool Registry
    ToolRegistry,
    newToolRegistry,
    registerTool,
    lookupTool,
    allTools,
    toolSchemas,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Elwood.Claude.Types (ToolName, ToolSchema (..))
import Elwood.Tools.Types (Tool (..))

-- | Registry of available tools
newtype ToolRegistry = ToolRegistry (Map ToolName Tool)

-- | Create an empty tool registry
newToolRegistry :: ToolRegistry
newToolRegistry = ToolRegistry Map.empty

-- | Register a tool in the registry
registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool tool (ToolRegistry reg) =
  ToolRegistry $ Map.insert tool.schema.name tool reg

-- | Look up a tool by name
lookupTool :: ToolName -> ToolRegistry -> Maybe Tool
lookupTool n (ToolRegistry reg) = Map.lookup n reg

-- | Get all registered tools
allTools :: ToolRegistry -> [Tool]
allTools (ToolRegistry reg) = Map.elems reg

-- | Generate tool schemas for the API request (all tools)
toolSchemas :: ToolRegistry -> [ToolSchema]
toolSchemas registry = map (.schema) (allTools registry)
