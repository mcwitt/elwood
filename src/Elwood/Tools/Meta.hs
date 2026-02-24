{-# LANGUAGE StrictData #-}

module Elwood.Tools.Meta
  ( searchTools,
  )
where

import Data.List (sortBy)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Registry (ToolRegistry, toolSchemas)

-- | Search the registry for tools matching a query.
--
-- Case-insensitive substring match on name and description.
-- Multi-word queries require all terms to match (AND logic).
-- Results ranked by how many terms match the tool name (vs description only),
-- capped at 20.
--
-- Returns (display text for the user, set of tool names to activate).
searchTools :: ToolRegistry -> Text -> (Text, Set Text)
searchTools registry query =
  let terms = map T.toLower (T.words query)
      schemas = toolSchemas registry
      matchesAll ts =
        let nameLower = T.toLower ts.name
            descLower = T.toLower ts.description
            combined = nameLower <> " " <> descLower
         in all (`T.isInfixOf` combined) terms
      scoreMatch ts =
        let nameLower = T.toLower ts.name
         in length (filter (`T.isInfixOf` nameLower) terms)
      allMatches = filter matchesAll schemas
      ranked = sortBy (\a b -> compare (Down (scoreMatch a)) (Down (scoreMatch b))) allMatches
      maxResults = 20
      totalCount = length ranked
      shown = take maxResults ranked
   in if null shown
        then ("No tools found matching your query. Try different keywords.", Set.empty)
        else
          let names = Set.fromList (map (.name) shown)
              header
                | totalCount > maxResults =
                    "Loaded "
                      <> T.pack (show maxResults)
                      <> " of "
                      <> T.pack (show totalCount)
                      <> " matching tools. Try a more specific query to find other tools.\n"
                | otherwise = ""
           in (header <> T.unlines (map (.name) shown), names)
