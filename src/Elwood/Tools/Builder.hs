module Elwood.Tools.Builder (tool, schema, field, text, nonempty, integer, optionalInteger, parse) where

import Data.Aeson (Key, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolName, ToolSchema (..))
import Elwood.Tools.Types (Tool (..), ToolResult, toolError)

tool :: ToolName -> Text -> Value -> (Value -> Parser a) -> (a -> IO ToolResult) -> Tool
tool name description inputSchema parser action = Tool (ToolSchema name description inputSchema) $ \value ->
  either (pure . toolError) action (parse parser value)

parse :: (Value -> Parser a) -> Value -> Either Text a
parse parser = either (Left . T.pack) Right . parseEither parser

schema :: [(Key, Value)] -> [Text] -> Value
schema fields required = object ["type" .= ("object" :: Text), "properties" .= object [key .= value | (key, value) <- fields], "required" .= required]

field :: Text -> Text -> Value
field kind description = object ["type" .= kind, "description" .= description]

text :: Aeson.Object -> Key -> Parser Text
text value name = value Aeson..: name

nonempty :: Aeson.Object -> Key -> Parser Text
nonempty value name = do
  result <- text value name
  if T.null (T.strip result) then fail (show name <> " must not be empty") else pure result

integer :: Int -> Int -> Value -> Parser Int
integer lower upper value = do
  result <- Aeson.parseJSON @Int value
  if result >= lower && result <= upper then pure result else fail ("integer must be between " <> show lower <> " and " <> show upper)

optionalInteger :: Aeson.Object -> Key -> Int -> Int -> Int -> Parser Int
optionalInteger value name fallback lower upper = do
  input <- value Aeson..:! name
  maybe (pure fallback) (integer lower upper) input
