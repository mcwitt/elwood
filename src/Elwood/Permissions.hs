{-# LANGUAGE StrictData #-}

module Elwood.Permissions
  ( -- * Permission Configuration
    PermissionConfig (..),
    defaultPermissionConfig,

    -- * Permission Checker
    PermissionChecker (..),
    newPermissionChecker,

    -- * Permission Results
    PermissionResult (..),
    isAllowed,

    -- * Permission Checks
    checkCommandPermission,
    checkPathPermission,
  )
where

import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (makeRelative, normalise)
import Text.Regex.TDFA ((=~))

-- | Configuration for permissions
data PermissionConfig = PermissionConfig
  { -- | Commands that are always allowed (prefix match)
    pcSafeCommands :: [Text],
    -- | Regex patterns that are always blocked
    pcDangerousPatterns :: [Text],
    -- | Relative paths allowed for file operations (relative to workspace)
    pcAllowedPaths :: [FilePath]
  }
  deriving stock (Show, Eq)

-- | Default permission configuration
defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig =
  PermissionConfig
    { pcSafeCommands =
        [ "ls",
          "cat",
          "head",
          "tail",
          "date",
          "whoami",
          "pwd",
          "git status",
          "git log",
          "git diff",
          "echo",
          "wc",
          "find",
          "grep"
        ],
      pcDangerousPatterns =
        [ "\\brm\\b",
          "\\bsudo\\b",
          "\\bchmod\\b",
          "\\bchown\\b",
          "curl.*\\|.*sh",
          "wget.*\\|.*sh",
          "\\bmkfs\\b",
          "\\bdd\\b.*of=/",
          "\\b>\\s*/dev/",
          "\\bformat\\b"
        ],
      pcAllowedPaths =
        [ "workspace",
          "scratch",
          "."
        ]
    }

-- | Result of a permission check
data PermissionResult
  = -- | Operation is permitted
    Allowed
  | -- | Operation is denied with reason
    Denied Text
  deriving stock (Show, Eq)

-- | Check if a permission result allows the operation
isAllowed :: PermissionResult -> Bool
isAllowed Allowed = True
isAllowed (Denied _) = False

-- | Permission checker with resolved paths
data PermissionChecker = PermissionChecker
  { -- | Raw configuration
    pcConfig :: PermissionConfig,
    -- | Workspace directory for resolving paths
    pcWorkspaceDir :: FilePath
  }

-- | Create a new permission checker
newPermissionChecker :: PermissionConfig -> FilePath -> PermissionChecker
newPermissionChecker config workspaceDir =
  PermissionChecker
    { pcConfig = config,
      pcWorkspaceDir = workspaceDir
    }

-- | Check if a command is allowed
checkCommand :: PermissionChecker -> Text -> PermissionResult
checkCommand checker cmd
  | isSafeCommand = Allowed
  | Just pat <- matchesDangerous = Denied $ "Command matches dangerous pattern: " <> pat
  | otherwise = Allowed -- Allow by default for M3
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
  | escapesWorkspace = Denied "Path escapes workspace directory"
  | isUnderAllowedPath = Allowed
  | otherwise = Denied $ "Path not under allowed directories: " <> T.pack relPath
  where
    workspace = pcWorkspaceDir checker
    config = pcConfig checker

    -- Make path relative to workspace
    relPath = makeRelative workspace (normalise path)

    -- Check for directory traversal attacks
    escapesWorkspace = ".." `T.isPrefixOf` T.pack relPath

    -- Check if path falls under any allowed directory
    isUnderAllowedPath = any isUnderAllowed (pcAllowedPaths config)

    isUnderAllowed :: FilePath -> Bool
    isUnderAllowed allowedDir
      | allowedDir == "." = True -- "." means entire workspace is allowed
      | otherwise = allowedDir == relPath || (allowedDir ++ "/") `isPrefixOf` relPath

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
