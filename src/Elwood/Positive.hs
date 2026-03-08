module Elwood.Positive
  ( Positive (getPositive),
  )
where

import Data.Aeson (FromJSON (..), withScientific)

-- | A positive integer (>= 1). Use numeric literals (via the 'Num' instance).
newtype Positive = UnsafePositive {getPositive :: Int}
  deriving stock (Show, Eq, Ord)

instance Num Positive where
  fromInteger n
    | n >= 1 = UnsafePositive (fromInteger n)
    | otherwise = error $ "Positive.fromInteger: expected >= 1, got " <> show n
  (+) = error "Positive: (+) not supported"
  (*) = error "Positive: (*) not supported"
  abs = error "Positive: abs not supported"
  signum = error "Positive: signum not supported"
  negate = error "Positive: negate not supported"

instance FromJSON Positive where
  parseJSON = withScientific "Positive" $ \s ->
    let n = round s
     in if n >= 1
          then pure (UnsafePositive n)
          else fail $ "Expected positive integer (>= 1), got " <> show n
