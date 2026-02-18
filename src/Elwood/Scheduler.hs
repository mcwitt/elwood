{-# LANGUAGE StrictData #-}

module Elwood.Scheduler
  ( SchedulerEnv (..),
    runScheduler,

    -- * Exported for testing
    isWithinActiveHours,
    hashJobName,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.Bits (shiftL, xor)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
    getCurrentTimeZone,
    localTimeOfDay,
    todHour,
    utcToLocalTime,
  )
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation (ConversationStore)
import Elwood.Config (CronJob (..), HeartbeatConfig (..))
import Elwood.Event
  ( DeliveryTarget (..),
    Event (..),
    EventEnv (..),
    EventSource (..),
    SessionConfig (..),
    handleEvent,
  )
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Telegram.Client (TelegramClient, notify)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (ToolEnv)

-- | Environment for the scheduler
data SchedulerEnv = SchedulerEnv
  { seLogger :: Logger,
    seTelegram :: TelegramClient,
    seClaude :: ClaudeClient,
    seConversations :: ConversationStore,
    seRegistry :: ToolRegistry,
    seToolEnv :: ToolEnv,
    seCompaction :: CompactionConfig,
    seSystemPrompt :: Maybe Text,
    seModel :: Text,
    seHeartbeatConfig :: HeartbeatConfig,
    -- | Contents of HEARTBEAT.md
    seHeartbeatPrompt :: Maybe Text,
    -- | Who to notify
    seNotifyChatIds :: [Int64],
    seCronJobs :: [CronJob]
  }

-- | Run the scheduler loop
runScheduler :: SchedulerEnv -> IO ()
runScheduler env = do
  logInfo (seLogger env) "Scheduler starting" []

  -- Initialize timing state
  now <- getCurrentTime
  let initialCronChecks = Map.fromList [(cjName job, now) | job <- seCronJobs env]

  schedulerLoop env now initialCronChecks

-- | Main scheduler loop
schedulerLoop :: SchedulerEnv -> UTCTime -> Map.Map Text UTCTime -> IO ()
schedulerLoop env lastHeartbeat cronChecks = do
  -- Sleep for 60 seconds
  threadDelay (60 * 1000000)

  now <- getCurrentTime
  let logger = seLogger env
      config = seHeartbeatConfig env

  -- Check if we should run heartbeat
  newLastHeartbeat <-
    if hbEnabled config
      then do
        inActiveHours <- withinActiveHours config
        let minutesSinceHeartbeat = realToFrac (diffUTCTime now lastHeartbeat) / 60.0 :: Double
            shouldRun = minutesSinceHeartbeat >= fromIntegral (hbIntervalMinutes config)

        if shouldRun && inActiveHours
          then do
            logInfo logger "Heartbeat check triggered" []
            runHeartbeat env
            pure now
          else do
            when (shouldRun && not inActiveHours) $
              logInfo logger "Outside active hours, skipping heartbeat" []
            pure lastHeartbeat
      else pure lastHeartbeat

  -- Check cron jobs
  newCronChecks <- checkCronJobs env now cronChecks

  -- Continue the loop
  schedulerLoop env newLastHeartbeat newCronChecks

-- | Check if a given hour is within active hours (pure function for testing)
isWithinActiveHours :: Int -> Int -> Int -> Bool
isWithinActiveHours startHour endHour currentHour =
  currentHour >= startHour && currentHour < endHour

-- | Check if current time is within active hours
withinActiveHours :: HeartbeatConfig -> IO Bool
withinActiveHours config = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
      hour = todHour (localTimeOfDay localTime)
  pure $ isWithinActiveHours (hbActiveHoursStart config) (hbActiveHoursEnd config) hour

-- | Run the heartbeat check
runHeartbeat :: SchedulerEnv -> IO ()
runHeartbeat env = do
  let logger = seLogger env

  case seHeartbeatPrompt env of
    Nothing -> do
      logWarn logger "No HEARTBEAT.md found, skipping" []
    Just heartbeatPromptText -> do
      logInfo logger "Running heartbeat" []

      let prompt =
            "Heartbeat check. Follow this checklist:\n"
              <> heartbeatPromptText
              <> "\nIf nothing needs attention, reply HEARTBEAT_OK."

      -- Create event environment
      let eventEnv = schedulerToEventEnv env

      -- Create heartbeat event
      now <- getCurrentTime
      let event =
            Event
              { evSource = HeartbeatSource,
                evTimestamp = now,
                evPayload = Null,
                evPrompt = prompt,
                evImage = Nothing,
                evSession = Named "heartbeat",
                -- Heartbeat only notifies if NOT OK, so use LogOnly for normal case
                -- We'll check the response and notify manually
                evDelivery = [LogOnly]
              }

      result <- handleEvent eventEnv event

      case result of
        Right responseText -> do
          -- Check if we need to notify
          if "HEARTBEAT_OK" `T.isInfixOf` responseText
            then logInfo logger "Heartbeat OK, no notification needed" []
            else do
              logInfo logger "Heartbeat needs attention, notifying" []
              mapM_ (\chatId -> notifySafe env chatId responseText) (seNotifyChatIds env)
        Left err -> do
          logError logger "Heartbeat failed" [("error", err)]

-- | Check and run due cron jobs
checkCronJobs :: SchedulerEnv -> UTCTime -> Map.Map Text UTCTime -> IO (Map.Map Text UTCTime)
checkCronJobs env now cronChecks =
  foldM checkJob cronChecks (seCronJobs env)
  where
    foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
    foldM _ acc [] = pure acc
    foldM f acc (x : xs) = do
      acc' <- f acc x
      foldM f acc' xs

    checkJob :: Map.Map Text UTCTime -> CronJob -> IO (Map.Map Text UTCTime)
    checkJob checks job = do
      let lastRun = Map.findWithDefault now (cjName job) checks
          minutesSinceRun = realToFrac (diffUTCTime now lastRun) / 60.0 :: Double

      if minutesSinceRun >= fromIntegral (cjIntervalMinutes job)
        then do
          runCronJob env job
          pure $ Map.insert (cjName job) now checks
        else pure checks

-- | Run a single cron job
runCronJob :: SchedulerEnv -> CronJob -> IO ()
runCronJob env job = do
  let logger = seLogger env
      jobName = cjName job

  logInfo logger "Running cron job" [("job", jobName)]

  -- Create event environment
  let eventEnv = schedulerToEventEnv env

  -- Create cron job event
  now <- getCurrentTime
  let sessionConfig =
        if cjIsolated job
          then Isolated
          else Named ("cron:" <> jobName)
      event =
        Event
          { evSource = CronSource jobName,
            evTimestamp = now,
            evPayload = Null,
            evPrompt = cjPrompt job,
            evImage = Nothing,
            evSession = sessionConfig,
            -- We handle delivery manually with job name prefix
            evDelivery = [LogOnly]
          }

  result <- handleEvent eventEnv event

  case result of
    Right responseText -> do
      -- Notify with job name prefix
      let notification = "[" <> jobName <> "] " <> responseText
      logInfo logger "Cron job completed" [("job", jobName)]
      mapM_ (\chatId -> notifySafe env chatId notification) (seNotifyChatIds env)
    Left err -> do
      logError logger "Cron job failed" [("job", jobName), ("error", err)]

-- | DJB2 hash function for Text (better distribution than XOR)
-- Used to generate unique session IDs for isolated cron jobs
hashJobName :: Text -> Int
hashJobName = T.foldl' step 5381
  where
    step :: Int -> Char -> Int
    step h c = ((h `shiftL` 5) + h) `xor` fromEnum c

-- | Convert SchedulerEnv to EventEnv
schedulerToEventEnv :: SchedulerEnv -> EventEnv
schedulerToEventEnv env =
  EventEnv
    { eeLogger = seLogger env,
      eeTelegram = seTelegram env,
      eeClaude = seClaude env,
      eeConversations = seConversations env,
      eeRegistry = seRegistry env,
      eeToolEnv = seToolEnv env,
      eeCompaction = seCompaction env,
      eeSystemPrompt = seSystemPrompt env,
      eeModel = seModel env,
      eeNotifyChatIds = seNotifyChatIds env
    }

-- | Safely send notification, catching any errors
notifySafe :: SchedulerEnv -> Int64 -> Text -> IO ()
notifySafe env chatId msg = do
  notify (seLogger env) (seTelegram env) chatId msg
    `catch` \(e :: SomeException) ->
      logError (seLogger env) "Failed to send notification" [("chat_id", T.pack (show chatId)), ("error", T.pack (show e))]
