module Test.Elwood.Permissions (tests) where

import Data.Text (Text)
import Elwood.Permissions
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Permissions"
    [commandPermissionTests]

commandPermissionTests :: TestTree
commandPermissionTests =
  testGroup
    "checkCommandPermission"
    [ testCase "allows safe pattern" $
        let cfg = mkCfg [] ["^ls\\b"]
         in checkCommandPermission cfg "ls -la" @?= Allowed,
      testCase "blocks dangerous pattern" $
        let cfg = mkCfg ["\\brm\\b"] []
         in assertDenied $ checkCommandPermission cfg "rm -rf /",
      testCase "safe overrides dangerous" $
        let cfg = mkCfg ["\\brm\\b"] ["^rm -i\\b"]
         in checkCommandPermission cfg "rm -i temp.txt" @?= Allowed,
      testCase "dangerous still blocks when no safe override" $
        let cfg = mkCfg ["\\brm\\b"] ["^rm -i\\b"]
         in assertDenied $ checkCommandPermission cfg "rm -rf /",
      testCase "allows unknown command with empty patterns" $
        checkCommandPermission defaultPermissionConfig "myunknowncommand" @?= Allowed,
      testCase "strips whitespace" $
        let cfg = mkCfg [] ["^ls\\b"]
         in checkCommandPermission cfg "  ls -la  " @?= Allowed,
      testCase "default config has empty patterns" $ do
        defaultPermissionConfig.safePatterns @?= []
        defaultPermissionConfig.dangerousPatterns @?= []
    ]

-- | Build a PermissionConfig with custom dangerous/safe patterns and defaults for the rest
mkCfg :: [Text] -> [Text] -> PermissionConfig
mkCfg dangerous safe =
  resolvePermissions $
    PermissionConfigFile (Just safe) (Just dangerous) Nothing Nothing Nothing

-- | Assert a PermissionResult is Denied
assertDenied :: PermissionResult -> IO ()
assertDenied (Denied _) = pure ()
assertDenied Allowed = assertFailure "Expected Denied but got Allowed"
