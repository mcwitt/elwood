module Elwood.Claude.OutputSchema (normalize) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM

-- | Supply Claude's closed-object default only at JSON Schema positions.
-- Explicit constraints and instance data (defaults, examples, enum, const)
-- are preserved; this does not validate the provider's supported vocabulary.
normalize :: Value -> Value
normalize (Object schema) = Object (KM.mapWithKey descend closed)
  where
    closed
      | objectType (KM.lookup "type" schema) && not (KM.member "additionalProperties" schema) = KM.insert "additionalProperties" (Bool False) schema
      | otherwise = schema
    objectType (Just (String "object")) = True
    objectType (Just (Array types)) = String "object" `elem` types
    objectType _ = False
    descend key value
      | key `elem` ["properties", "patternProperties", "$defs", "definitions", "dependentSchemas", "dependencies"] = case value of
          Object schemas -> Object (fmap normalize schemas)
          _ -> value
      | key `elem` ["allOf", "anyOf", "oneOf", "prefixItems"] = case value of
          Array schemas -> Array (fmap normalize schemas)
          _ -> value
      | key == "items" = case value of
          Array schemas -> Array (fmap normalize schemas)
          _ -> normalize value
      | key `elem` ["additionalProperties", "unevaluatedProperties", "propertyNames", "additionalItems", "unevaluatedItems", "contains", "not", "if", "then", "else", "contentSchema"] = normalize value
      | otherwise = value
normalize value = value
