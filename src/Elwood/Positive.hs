module Elwood.Positive
  ( Positive (getPositive),
    unsafePositive,
  )
where

import Data.Aeson (FromJSON (..), withScientific)

-- | A positive integer (>= 1). Constructor not exported; use 'unsafePositive'.
newtype Positive = UnsafePositive {getPositive :: Int}
  deriving stock (Show, Eq, Ord)

-- | Construct from a known-good literal. Errors on <= 0.
unsafePositive :: Int -> Positive
unsafePositive n
  | n >= 1 = UnsafePositive n
  | otherwise = error $ "unsafePositive: expected >= 1, got " <> show n

instance FromJSON Positive where
  parseJSON = withScientific "Positive" $ \s ->
    let n = round s
     in if n >= 1
          then pure (UnsafePositive n)
          else fail $ "Expected positive integer (>= 1), got " <> show n
