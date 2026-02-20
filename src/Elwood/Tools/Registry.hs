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
    alwaysLoadedNames,
    hasDynamicTools,

    -- * Active Tool Set
    ActiveToolSet,
    newActiveToolSet,
    activateTool,
    isToolActive,
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

-- | Get names of all AlwaysLoaded tools
alwaysLoadedNames :: ToolRegistry -> Set Text
alwaysLoadedNames (ToolRegistry reg) =
  Map.keysSet $ Map.filter (\(_, cat) -> cat == AlwaysLoaded) reg

-- | Whether the registry contains any DynamicLoadable tools
hasDynamicTools :: ToolRegistry -> Bool
hasDynamicTools (ToolRegistry reg) =
  any (\(_, cat) -> cat == DynamicLoadable) reg

-- | Tracks which tools are active (schemas sent) in the current turn
data ActiveToolSet = ActiveToolSet
  { -- | Tools that are always included
    atsAlwaysLoaded :: Set Text,
    -- | Tools dynamically loaded this turn
    atsLoaded :: Set Text
  }
  deriving stock (Show, Eq)

-- | Create a new active tool set from the always-loaded names
newActiveToolSet :: Set Text -> ActiveToolSet
newActiveToolSet always =
  ActiveToolSet
    { atsAlwaysLoaded = always,
      atsLoaded = Set.empty
    }

-- | Add a tool to the dynamically loaded set
activateTool :: Text -> ActiveToolSet -> ActiveToolSet
activateTool name ats = ats {atsLoaded = Set.insert name (atsLoaded ats)}

-- | Check whether a tool is active (either always-loaded or dynamically loaded)
isToolActive :: Text -> ActiveToolSet -> Bool
isToolActive name ats =
  Set.member name (atsAlwaysLoaded ats) || Set.member name (atsLoaded ats)

-- | Generate tool schemas for only the active tools
activeToolSchemas :: ToolRegistry -> ActiveToolSet -> [ToolSchema]
activeToolSchemas registry ats =
  [ ToolSchema
      { tsName = toolName tool,
        tsDescription = toolDescription tool,
        tsInputSchema = toolInputSchema tool
      }
  | (name, (tool, _)) <- Map.toList (trTools registry),
    isToolActive name ats
  ]
