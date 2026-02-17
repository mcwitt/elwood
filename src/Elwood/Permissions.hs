{-# LANGUAGE StrictData #-}

module Elwood.Permissions
  ( -- * Permission Configuration
    PermissionConfig (..)
  , defaultPermissionConfig

    -- * Permission Checker
  , PermissionChecker (..)
  , newPermissionChecker

    -- * Permission Results
  , PermissionResult (..)
  , isAllowed

    -- * Permission Checks
  , checkCommandPermission
  , checkPathPermission
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (makeRelative, normalise, (</>))
import Text.Regex.TDFA ((=~))

-- | Configuration for permissions
data PermissionConfig = PermissionConfig
  { pcSafeCommands :: [Text]
  -- ^ Commands that are always allowed (prefix match)
  , pcDangerousPatterns :: [Text]
  -- ^ Regex patterns that are always blocked
  , pcAllowedPaths :: [FilePath]
  -- ^ Relative paths allowed for file operations (relative to workspace)
  }
  deriving stock (Show, Eq)

-- | Default permission configuration
defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig =
  PermissionConfig
    { pcSafeCommands =
        [ "ls"
        , "cat"
        , "head"
        , "tail"
        , "date"
        , "whoami"
        , "pwd"
        , "git status"
        , "git log"
        , "git diff"
        , "echo"
        , "wc"
        , "find"
        , "grep"
        ]
    , pcDangerousPatterns =
        [ "\\brm\\b"
        , "\\bsudo\\b"
        , "\\bchmod\\b"
        , "\\bchown\\b"
        , "curl.*\\|.*sh"
        , "wget.*\\|.*sh"
        , "\\bmkfs\\b"
        , "\\bdd\\b.*of=/"
        , "\\b>\\s*/dev/"
        , "\\bformat\\b"
        ]
    , pcAllowedPaths =
        [ "workspace"
        , "scratch"
        , "."
        ]
    }

-- | Result of a permission check
data PermissionResult
  = Allowed
  -- ^ Operation is permitted
  | Denied Text
  -- ^ Operation is denied with reason
  deriving stock (Show, Eq)

-- | Check if a permission result allows the operation
isAllowed :: PermissionResult -> Bool
isAllowed Allowed = True
isAllowed (Denied _) = False

-- | Permission checker with resolved paths
data PermissionChecker = PermissionChecker
  { pcConfig :: PermissionConfig
  -- ^ Raw configuration
  , pcWorkspaceDir :: FilePath
  -- ^ Workspace directory for resolving paths
  }

-- | Create a new permission checker
newPermissionChecker :: PermissionConfig -> FilePath -> PermissionChecker
newPermissionChecker config workspaceDir =
  PermissionChecker
    { pcConfig = config
    , pcWorkspaceDir = workspaceDir
    }

-- | Check if a command is allowed
checkCommand :: PermissionChecker -> Text -> PermissionResult
checkCommand checker cmd
  | isSafeCommand = Allowed
  | Just pattern <- matchesDangerous = Denied $ "Command matches dangerous pattern: " <> pattern
  | otherwise = Allowed  -- Allow by default for M3
  where
    config = pcConfig checker
    cmdText = T.strip cmd

    -- Check if command starts with a safe command
    isSafeCommand = any (`T.isPrefixOf` cmdText) (pcSafeCommands config)

    -- Check if command matches any dangerous pattern
    matchesDangerous =
      let patterns = pcDangerousPatterns config
          cmdStr = T.unpack cmdText
       in case filter (\p -> cmdStr =~ T.unpack p) patterns of
            (p : _) -> Just p
            [] -> Nothing

-- | Check if a file path is allowed for reading/writing
checkFilePath :: PermissionChecker -> FilePath -> PermissionResult
checkFilePath checker path
  | isUnderAllowedPath = Allowed
  | otherwise = Denied $ "Path not under allowed directories: " <> T.pack path
  where
    workspace = pcWorkspaceDir checker
    config = pcConfig checker

    -- Normalise the path and make it relative to workspace
    normPath = normalise path
    relPath = makeRelative workspace normPath

    -- Check if the path is under any allowed directory
    isUnderAllowedPath =
      -- Path must not escape workspace (no leading ..)
      not (".." `T.isPrefixOf` T.pack relPath)
        && any (isUnderDir relPath) (pcAllowedPaths config)

    isUnderDir :: FilePath -> FilePath -> Bool
    isUnderDir _targetPath allowedDir =
      let normAllowed = normalise (workspace </> allowedDir)
          normTarget = normalise (workspace </> relPath)
       in normAllowed `isPrefixOfPath` normTarget
          || relPath == allowedDir
          || ("." `T.isPrefixOf` T.pack relPath && "." `elem` pcAllowedPaths config)

    isPrefixOfPath :: FilePath -> FilePath -> Bool
    isPrefixOfPath prefix target =
      T.pack prefix `T.isPrefixOf` T.pack target

-- | Instance for checking different operation types
instance Show PermissionChecker where
  show pc = "PermissionChecker { workspace = " <> pcWorkspaceDir pc <> " }"

-- Export the check functions as methods that can be called on PermissionChecker

-- | Check if running a command is allowed
checkCommandPermission :: PermissionChecker -> Text -> PermissionResult
checkCommandPermission = checkCommand

-- | Check if accessing a file path is allowed
checkPathPermission :: PermissionChecker -> FilePath -> PermissionResult
checkPathPermission = checkFilePath

-- Re-export as part of PermissionChecker usage pattern
-- Users call: checkCommandPermission checker "ls -la"
-- or: checkPathPermission checker "/some/path"
