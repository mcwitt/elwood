{-# LANGUAGE StrictData #-}

module Elwood.Tools.Registry
  ( -- * Tool Registry
    ToolRegistry
  , newToolRegistry
  , registerTool
  , lookupTool
  , allTools
  , toolSchemas
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Types (Tool (..))

-- | Registry of available tools
newtype ToolRegistry = ToolRegistry
  { unRegistry :: Map Text Tool
  }

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

-- | Generate tool schemas for the API request
toolSchemas :: ToolRegistry -> [ToolSchema]
toolSchemas registry =
  [ ToolSchema
      { tsName = toolName tool
      , tsDescription = toolDescription tool
      , tsInputSchema = toolInputSchema tool
      }
  | tool <- allTools registry
  ]
