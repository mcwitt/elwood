module Elwood.Tools.Registry
  ( ToolRegistry,
    newToolRegistry,
    registerTool,
    lookupTool,
    allTools,
    toolSchemas,
    filterRegistry,
    applyToolFilter,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Elwood.AgentSettings (ToolFilter (..))
import Elwood.Claude.Types (ToolName, ToolSchema (..))
import Elwood.Tools.Types (Tool (..))

-- Composition is right biased: later registrations replace earlier definitions.
newtype ToolRegistry = ToolRegistry (Map.Map ToolName Tool)

instance Semigroup ToolRegistry where
  ToolRegistry earlier <> ToolRegistry later = ToolRegistry (Map.union later earlier)

instance Monoid ToolRegistry where
  mempty = ToolRegistry Map.empty

newToolRegistry :: ToolRegistry
newToolRegistry = mempty

registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool tool registry = registry <> ToolRegistry (Map.singleton tool.schema.name tool)

lookupTool :: ToolName -> ToolRegistry -> Maybe Tool
lookupTool name (ToolRegistry tools) = Map.lookup name tools

allTools :: ToolRegistry -> [Tool]
allTools (ToolRegistry tools) = Map.elems tools

toolSchemas :: ToolRegistry -> [ToolSchema]
toolSchemas = map (.schema) . allTools

filterRegistry :: (ToolName -> Bool) -> ToolRegistry -> ToolRegistry
filterRegistry keep (ToolRegistry tools) = ToolRegistry (Map.filterWithKey (\name _ -> keep name) tools)

applyToolFilter :: ToolFilter -> ToolRegistry -> ToolRegistry
applyToolFilter AllTools = id
applyToolFilter (OnlyTools names) = filterRegistry (`Set.member` names)
