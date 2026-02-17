module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Elwood.App (runApp)
import Elwood.Config (loadConfig)

main :: IO ()
main = do
  -- Get config file path from environment or use default
  configPath <- fromMaybe "config.yaml" <$> lookupEnv "ELWOOD_CONFIG"

  -- Load configuration
  config <- loadConfig configPath

  -- Run the application
  runApp config
