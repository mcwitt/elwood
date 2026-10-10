{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Bounded positive integers. Runtime input is parsed; static bounds are proved.
module Elwood.Positive (Positive (getPositive), positive, literal) where

import Data.Aeson (FromJSON (..), ToJSON)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, natVal, type (<=))

newtype Positive = Positive {getPositive :: Int}
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

positive :: Int -> Maybe Positive
positive n
  | n > 0 = Just (Positive n)
  | otherwise = Nothing

-- | Portable checked constants, with no partial Num instance.
literal :: forall n. (KnownNat n, 1 <= n, n <= 2147483647) => Positive
literal = Positive (fromIntegral (natVal (Proxy @n)))

instance FromJSON Positive where
  parseJSON value = parseJSON @Int value >>= maybe (fail "Expected positive integer") pure . positive
