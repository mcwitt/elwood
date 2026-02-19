module Elwood.Aeson
  ( rejectUnknownKeys,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Set qualified as Set

-- | Fail if an object contains keys not in the given set
rejectUnknownKeys :: String -> [Key] -> Object -> Parser ()
rejectUnknownKeys name knownKeys obj =
  let unknown = Set.toList $ Set.difference (Set.fromList (KM.keys obj)) (Set.fromList knownKeys)
   in case unknown of
        [] -> pure ()
        ks -> fail $ name <> ": unknown keys: " <> show (map Key.toText ks)
