{-# LANGUAGE StrictData #-}

module Elwood.Tools.Registry
  ( -- * Tool Registry
    ToolRegistry,
    newToolRegistry,
    registerTool,
    lookupTool,
    allTools,
    toolSchemas,

    -- * Active Tool Set
    ActiveToolSet,
    newActiveToolSet,
    activateTool,
    activeToolSchemas,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Types (Tool (..))

-- | Registry of available tools
newtype ToolRegistry = ToolRegistry (Map Text Tool)

-- | Create an empty tool registry
newToolRegistry :: ToolRegistry
newToolRegistry = ToolRegistry Map.empty

-- | Register a tool in the registry
registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool tool (ToolRegistry reg) =
  ToolRegistry $ Map.insert (toolName tool) tool reg

-- | Look up a tool by name
lookupTool :: Text -> ToolRegistry -> Maybe Tool
lookupTool name (ToolRegistry reg) = Map.lookup name reg

-- | Get all registered tools
allTools :: ToolRegistry -> [Tool]
allTools (ToolRegistry reg) = Map.elems reg

-- | Generate tool schemas for the API request (all tools)
toolSchemas :: ToolRegistry -> [ToolSchema]
toolSchemas registry =
  [ ToolSchema
      { tsName = toolName tool,
        tsDescription = toolDescription tool,
        tsInputSchema = toolInputSchema tool
      }
  | tool <- allTools registry
  ]

-- | Tracks which tools are active for the current turn.
newtype ActiveToolSet = ActiveToolSet
  { atsActive :: Set Text
  }
  deriving stock (Show, Eq)

-- | Create an empty active tool set
newActiveToolSet :: ActiveToolSet
newActiveToolSet = ActiveToolSet Set.empty

-- | Add a tool to the active set
activateTool :: Text -> ActiveToolSet -> ActiveToolSet
activateTool name (ActiveToolSet loaded) = ActiveToolSet (Set.insert name loaded)

-- | Generate tool schemas for only the active tools.
-- A tool is active if its name is in the 'ActiveToolSet'.
activeToolSchemas :: ToolRegistry -> ActiveToolSet -> [ToolSchema]
activeToolSchemas (ToolRegistry reg) (ActiveToolSet loaded) =
  [ ToolSchema
      { tsName = toolName tool,
        tsDescription = toolDescription tool,
        tsInputSchema = toolInputSchema tool
      }
  | (name, tool) <- Map.toList reg,
    Set.member name loaded
  ]
