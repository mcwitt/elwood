-- | A regular expression compiled at the configuration boundary.
module Elwood.Pattern (Pattern, parse, matches, source) where

import Data.Aeson (FromJSON (..), withText)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Regex.TDFA (Regex, defaultCompOpt, defaultExecOpt, matchTest)
import Text.Regex.TDFA.String (compile)

data Pattern = Pattern {text :: Text, regex :: Regex}

instance Eq Pattern where
  a == b = a.text == b.text

instance Show Pattern where
  show = show . (.text)

instance FromJSON Pattern where
  parseJSON = withText "regular expression" (either fail pure . parse)

parse :: Text -> Either String Pattern
parse text = either (Left . ("Invalid regex: " <>)) (Right . Pattern text) (compile defaultCompOpt defaultExecOpt (Text.unpack text))

matches :: Pattern -> Text -> Bool
matches compiled = matchTest compiled.regex . Text.unpack

source :: Pattern -> Text
source = (.text)
