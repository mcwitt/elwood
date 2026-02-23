module Elwood.Logging
  ( LogLevel (..),
    LogMsg (..),
    Logger,
    newLogger,
    logMsg,
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Colog.Core (LogAction (..))
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)

-- | Log severity levels
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Show, Eq, Ord)

-- | Structured log message
data LogMsg = LogMsg
  { level :: LogLevel,
    message :: Text,
    context :: [(Text, Text)]
  }
  deriving stock (Show)

-- | Logger type alias using co-log-core
type Logger = LogAction IO LogMsg

-- | Format a log level for display
formatLevel :: LogLevel -> Text
formatLevel = \case
  Debug -> "DEBUG"
  Info -> "INFO "
  Warn -> "WARN "
  Error -> "ERROR"

-- | Format context pairs for display
formatContext :: [(Text, Text)] -> Text
formatContext [] = ""
formatContext ctx =
  " " <> T.intercalate " " [k <> "=" <> v | (k, v) <- ctx]

-- | Create a new logger that writes to stdout
newLogger :: LogLevel -> IO Logger
newLogger minLevel = pure $ LogAction $ \msg ->
  when (msg.level >= minLevel) $ do
    timestamp <- getCurrentTime
    let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" timestamp
        line =
          T.concat
            [ "[" <> timeStr <> "] ",
              "[" <> formatLevel msg.level <> "] ",
              msg.message,
              formatContext msg.context
            ]
    TIO.putStrLn line
    hFlush stdout

-- | Log a message with the given level and context
logMsg :: Logger -> LogLevel -> Text -> [(Text, Text)] -> IO ()
logMsg (LogAction action) lvl msg ctx =
  action LogMsg {level = lvl, message = msg, context = ctx}

-- | Log a debug message
logDebug :: Logger -> Text -> [(Text, Text)] -> IO ()
logDebug logger = logMsg logger Debug

-- | Log an info message
logInfo :: Logger -> Text -> [(Text, Text)] -> IO ()
logInfo logger = logMsg logger Info

-- | Log a warning message
logWarn :: Logger -> Text -> [(Text, Text)] -> IO ()
logWarn logger = logMsg logger Warn

-- | Log an error message
logError :: Logger -> Text -> [(Text, Text)] -> IO ()
logError logger = logMsg logger Error
