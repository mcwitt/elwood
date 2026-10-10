module Test.Elwood.Boundary (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Exception (SomeException, try)
import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Text qualified as T
import Elwood.Command (CommandResult (..), runCommand, runCommandWithTimeout)
import Elwood.Positive (Positive)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Boundary contracts"
    [ testCase "positive integers reject fractions, zero, negative and overflow" $
        mapM_ rejects [Number 1.5, Number 0, Number (-1), Number 18446744073709551617],
      testCase "positive integers accept ordinary values" $
        case fromJSON @Positive (Number 20) of
          Success _ -> pure ()
          Error e -> assertFailure e,
      testCase "shell drains stdout and stderr concurrently" $ do
        result <- timeout 5000000 $ runCommand "head -c 200000 /dev/zero >&2; printf done" "/tmp"
        case result of
          Just r -> do
            r.exitCode @?= ExitSuccess
            assertBool "stdout preserved" ("done" `T.isInfixOf` r.output)
          Nothing -> assertFailure "pipe deadlock",
      testCase "command deadline is reported as timeout" $ do
        result <- runCommandWithTimeout "sleep 5" 1 "/tmp"
        result.exitCode @?= ExitFailure 124,
      testCase "cancellation is not converted to a command error" $ do
        worker <- async $ runCommand "sleep 5" "/tmp"
        cancel worker
        waitCatch worker >>= \case
          Left _ -> pure ()
          Right _ -> assertFailure "cancellation swallowed",
      testCase "timeout terminates shell descendants" $
        withSystemTempDirectory "command-lifetime" $ \dir -> do
          _ <- runCommandWithTimeout "(sleep 2; touch survived) & wait" 1 dir
          threadDelay 1500000
          exists <- doesFileExist (dir </> "survived")
          assertBool "child escaped command lifetime" (not exists),
      testCase "nonpositive command timeout is rejected" $ do
        result <- try @SomeException $ runCommandWithTimeout "printf ran" 0 "/tmp"
        case result of
          Left _ -> pure ()
          Right r -> assertBool "must not execute" (r.exitCode /= ExitSuccess)
    ]
  where
    rejects value = case fromJSON @Positive value of
      Error _ -> pure ()
      Success _ -> assertFailure ("accepted invalid integer " <> show value)
