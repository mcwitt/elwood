module Test.Elwood.Claude.OutputSchema (tests) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import Elwood.Claude.OutputSchema qualified as OutputSchema
import Elwood.Claude.Types (jsonSchemaFormat)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Claude.OutputSchema"
    [ testCase "output format closes an ordinary object schema" $ do
        let fields = ["properties" .= object ["answer" .= object ["type" .= String "integer"]], "required" .= [String "answer"]]
        toJSON (jsonSchemaFormat (openObject fields))
          @?= object ["type" .= String "json_schema", "schema" .= closedObject fields],
      testCase "nested properties and array elements are closed" $ do
        let nested make = make ["properties" .= object ["rows" .= object ["type" .= String "array", "items" .= make ["properties" .= object ["type" .= make []]]]]]
        OutputSchema.normalize (nested openObject) @?= nested closedObject,
      testCase "object in a nullable type union is closed" $
        OutputSchema.normalize (object ["type" .= [String "object", String "null"]])
          @?= object ["type" .= [String "object", String "null"], "additionalProperties" .= False],
      testCase "references retain their target while definitions are normalized" $ do
        let schema make = object ["$ref" .= String "#/$defs/Result", "$defs" .= object ["Result" .= make []]]
        OutputSchema.normalize (schema openObject) @?= schema closedObject,
      testGroup
        "schema maps"
        [ testCase (show keyword) $ do
            let schema make = object [keyword .= object ["type" .= make []]]
            OutputSchema.normalize (schema openObject) @?= schema closedObject
        | keyword <- ["properties", "patternProperties", "$defs", "definitions", "dependentSchemas", "dependencies"]
        ],
      testGroup
        "schema arrays"
        [ testCase (show keyword) $ do
            let schema make = object [keyword .= [make [], Bool True, object ["type" .= String "string"]]]
            OutputSchema.normalize (schema openObject) @?= schema closedObject
        | keyword <- ["allOf", "anyOf", "oneOf", "prefixItems", "items"]
        ],
      testGroup
        "single schemas"
        [ testCase (show keyword) $
            OutputSchema.normalize (object [keyword .= openObject []])
              @?= object [keyword .= closedObject []]
        | keyword <- ["items", "additionalProperties", "unevaluatedProperties", "propertyNames", "additionalItems", "unevaluatedItems", "contains", "not", "if", "then", "else", "contentSchema"]
        ],
      testCase "explicit additionalProperties booleans are preserved" $
        mapM_ (\allowed -> let schema = openObject ["additionalProperties" .= allowed] in OutputSchema.normalize schema @?= schema) [False, True],
      testCase "schema-valued additionalProperties retains its constraints" $
        OutputSchema.normalize (openObject ["additionalProperties" .= openObject ["minProperties" .= (1 :: Int)]])
          @?= openObject ["additionalProperties" .= closedObject ["minProperties" .= (1 :: Int)]],
      testCase "arbitrary instance data and extension keywords remain untouched" $ do
        let datum = openObject ["properties" .= object ["nested" .= openObject []]]
            fields = ["const" .= datum, "enum" .= [datum], "default" .= datum, "examples" .= [datum], "x-extension" .= datum]
        OutputSchema.normalize (openObject fields) @?= closedObject fields,
      testCase "unsupported constraints are retained instead of silently weakened" $ do
        let schema = object ["type" .= String "integer", "minimum" .= (1 :: Int), "maximum" .= (10 :: Int)]
        OutputSchema.normalize schema @?= schema,
      testCase "property dependencies remain instance names" $ do
        let schema = object ["dependencies" .= object ["type" .= [String "object", String "properties"]]]
        OutputSchema.normalize schema @?= schema,
      testCase "normalization is idempotent" $ do
        let schema = openObject ["properties" .= object ["nested" .= openObject []], "anyOf" .= [openObject [], Bool False]]
        OutputSchema.normalize (OutputSchema.normalize schema) @?= OutputSchema.normalize schema,
      testCase "boolean schemas remain unchanged" $
        mapM_ (\schema -> OutputSchema.normalize schema @?= schema) [Bool True, Bool False]
    ]

openObject :: [Pair] -> Value
openObject fields = object (["type" .= String "object"] <> fields)

closedObject :: [Pair] -> Value
closedObject fields = openObject (["additionalProperties" .= False] <> fields)
