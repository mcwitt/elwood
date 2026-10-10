module Elwood.Aeson (rejectUnknownKeys, recordOptions, enum, field) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- | Strict snake_case record codecs, including fields escaped with a trailing '_'.
recordOptions :: Options
recordOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . reverse . dropWhile (== '_') . reverse,
      rejectUnknownFields = True
    }

rejectUnknownKeys :: String -> [Key] -> Object -> Parser ()
rejectUnknownKeys name keys value = case filter (`notElem` keys) (KeyMap.keys value) of
  [] -> pure ()
  unknown -> fail (name <> ": unknown keys: " <> show (map Key.toText unknown))

enum :: String -> [(Text, a)] -> Value -> Parser a
enum name choices = withText name $ \t -> maybe (fail ("Unknown " <> name <> ": " <> show t)) pure (lookup (Text.toLower t) choices)

field :: (FromJSON a) => Object -> Key -> Parser (Last a)
field value key = Last <$> value .:? key
