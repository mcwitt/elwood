module Elwood.Logging (LogLevel (..), LogMsg (..), Logger, newLogger, logMsg, logInfo, logWarn, logError) where

import Colog.Core (LogAction (..))
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)

data LogLevel = Debug | Info | Warn | Error deriving stock (Show, Eq, Ord)

data LogMsg = LogMsg {level :: LogLevel, message :: Text, context :: [(Text, Text)]} deriving stock (Show)

type Logger = LogAction IO LogMsg

newLogger :: LogLevel -> IO Logger
newLogger minimumLevel = pure $ LogAction $ \entry -> when (entry.level >= minimumLevel) $ do
  now <- getCurrentTime
  T.putStrLn $
    T.intercalate " " $
      [ "[" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now) <> "]",
        "[" <> T.toUpper (T.pack (show entry.level)) <> "]",
        entry.message
      ]
        <> [key <> "=" <> value | (key, value) <- entry.context]
  hFlush stdout

logMsg :: Logger -> LogLevel -> Text -> [(Text, Text)] -> IO ()
logMsg (LogAction write) level message context = write (LogMsg level message context)

logInfo, logWarn, logError :: Logger -> Text -> [(Text, Text)] -> IO ()
logInfo logger = logMsg logger Info
logWarn logger = logMsg logger Warn
logError logger = logMsg logger Error
