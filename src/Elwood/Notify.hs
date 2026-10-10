module Elwood.Notify (truncateText) where

import Data.Text (Text)
import Data.Text qualified as T

truncateText :: Int -> Text -> Text
truncateText limit text = T.take limit stripped <> if T.length stripped > limit then "…" else ""
  where
    stripped = T.strip text
