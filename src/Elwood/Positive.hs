module Elwood.Positive
  ( Positive,
    unPositive,
    unsafePositive,
  )
where

import Data.Aeson (FromJSON (..), withScientific)

-- | A positive integer (>= 1). Constructor not exported; use 'unsafePositive'.
newtype Positive = UnsafePositive Int
  deriving stock (Show, Eq, Ord)

-- | Unwrap to 'Int'.
unPositive :: Positive -> Int
unPositive (UnsafePositive n) = n

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
