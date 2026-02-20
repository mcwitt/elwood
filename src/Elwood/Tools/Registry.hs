{-# LANGUAGE StrictData #-}

module Elwood.Tools.Registry
  ( -- * Tool Categories
    ToolCategory (..),

    -- * Tool Registry
    ToolRegistry,
    newToolRegistry,
    registerTool,
    registerToolWith,
    lookupTool,
    allTools,
    toolSchemas,
    hasDynamicTools,

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

-- | Category controlling whether a tool is always sent or loaded on demand
data ToolCategory = AlwaysLoaded | DynamicLoadable
  deriving stock (Show, Eq)

-- | Registry of available tools
newtype ToolRegistry = ToolRegistry
  { trTools :: Map Text (Tool, ToolCategory)
  }

-- | Create an empty tool registry
newToolRegistry :: ToolRegistry
newToolRegistry = ToolRegistry Map.empty

-- | Register a tool in the registry (defaults to AlwaysLoaded)
registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool = registerToolWith AlwaysLoaded

-- | Register a tool with an explicit category
registerToolWith :: ToolCategory -> Tool -> ToolRegistry -> ToolRegistry
registerToolWith cat tool (ToolRegistry reg) =
  ToolRegistry $ Map.insert (toolName tool) (tool, cat) reg

-- | Look up a tool by name
lookupTool :: Text -> ToolRegistry -> Maybe Tool
lookupTool name (ToolRegistry reg) = fst <$> Map.lookup name reg

-- | Get all registered tools
allTools :: ToolRegistry -> [Tool]
allTools (ToolRegistry reg) = map fst (Map.elems reg)

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

-- | Whether the registry contains any DynamicLoadable tools
hasDynamicTools :: ToolRegistry -> Bool
hasDynamicTools (ToolRegistry reg) =
  any (\(_, cat) -> cat == DynamicLoadable) reg

-- | Tracks which tools have been dynamically loaded in the current turn.
-- Always-loaded tools are determined by the registry's 'ToolCategory',
-- not duplicated here.
newtype ActiveToolSet = ActiveToolSet
  { atsLoaded :: Set Text
  }
  deriving stock (Show, Eq)

-- | Create an empty active tool set
newActiveToolSet :: ActiveToolSet
newActiveToolSet = ActiveToolSet Set.empty

-- | Add a tool to the dynamically loaded set
activateTool :: Text -> ActiveToolSet -> ActiveToolSet
activateTool name (ActiveToolSet loaded) = ActiveToolSet (Set.insert name loaded)

-- | Generate tool schemas for only the active tools.
-- A tool is active if it is 'AlwaysLoaded' in the registry or has been
-- dynamically loaded into the 'ActiveToolSet'.
activeToolSchemas :: ToolRegistry -> ActiveToolSet -> [ToolSchema]
activeToolSchemas (ToolRegistry reg) (ActiveToolSet loaded) =
  [ ToolSchema
      { tsName = toolName tool,
        tsDescription = toolDescription tool,
        tsInputSchema = toolInputSchema tool
      }
  | (name, (tool, cat)) <- Map.toList reg,
    cat == AlwaysLoaded || Set.member name loaded
  ]
