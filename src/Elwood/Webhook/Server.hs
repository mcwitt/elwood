module Elwood.Webhook.Server
  ( -- * Server
    runWebhookServer,
    webhookApp,

    -- * Template Rendering
    renderTemplate,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (SomeException, catch, handle)
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Elwood.AgentSettings (resolveProfile, toOverrides)
import Elwood.Event (AppEnv (..), BufferedResult (..), DeliveryTarget (..), Event (..), EventSource (..), handleEvent, handleEventBuffered)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (renderMetrics)
import Elwood.Prompt (assemblePrompt)
import Elwood.Webhook.Types (WebhookConfig (..), WebhookServerConfig (..))
import Network.HTTP.Types
  ( HeaderName,
    Status,
    hContentType,
    mkStatus,
    status200,
    status202,
    status401,
    status404,
    status500,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestHeaders,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)

-- | Header name for webhook secret
webhookSecretHeader :: HeaderName
webhookSecretHeader = "X-Webhook-Secret"

-- | Start the webhook server
runWebhookServer :: WebhookServerConfig -> AppEnv -> IO ()
runWebhookServer config env = do
  let lgr = env.logger
      p = config.port

  logInfo lgr "Starting webhook server" [("port", T.pack (show p))]

  run p (webhookApp config env)
    `catch` \(e :: SomeException) ->
      logError lgr "Webhook server error" [("error", T.pack (show e))]

-- | WAI application for webhook handling
webhookApp :: WebhookServerConfig -> AppEnv -> Application
webhookApp config env request respond = do
  let lgr = env.logger
      path = pathInfo request

  case path of
    ["webhook", webhookName] -> do
      -- Find matching webhook config
      case findWebhook webhookName config.webhooks of
        Nothing -> do
          logWarn lgr "Unknown webhook" [("name", webhookName)]
          respond $ jsonResponse status404 $ errorJson "Unknown webhook"
        Just webhookCfg -> do
          -- Verify authentication
          case config.secret of
            Just sec -> do
              let providedSecret = lookup webhookSecretHeader (requestHeaders request)
              if providedSecret == Just (TE.encodeUtf8 sec)
                then handleWebhookRequest lgr webhookCfg env request respond
                else do
                  logWarn lgr "Invalid webhook secret" [("name", webhookName)]
                  respond $ jsonResponse status401 $ errorJson "Invalid or missing secret"
            Nothing ->
              -- No secret required
              handleWebhookRequest lgr webhookCfg env request respond
    ["health"] -> do
      -- Health check endpoint
      respond $ jsonResponse status200 "{\"status\": \"ok\"}"
    ["metrics"] -> do
      -- Prometheus metrics endpoint
      body <- renderMetrics env.metrics env.conversations env.registry
      respond $ responseLBS status200 [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")] body
    _ -> do
      logWarn lgr "Unknown path" [("path", T.intercalate "/" path)]
      respond $ jsonResponse status404 $ errorJson "Not found"

-- | Handle a webhook request after authentication
--
-- Returns 202 Accepted immediately and processes the event asynchronously.
-- Results are delivered via the configured delivery target (e.g. Telegram),
-- not in the HTTP response.
handleWebhookRequest :: Logger -> WebhookConfig -> AppEnv -> Request -> (Response -> IO a) -> IO a
handleWebhookRequest lgr webhookCfg env request respond = do
  -- Read and parse request body
  body <- strictRequestBody request

  case eitherDecode body of
    Left err -> do
      logWarn lgr "Invalid JSON body" [("error", T.pack err)]
      respond $ jsonResponse status400 $ errorJson "Invalid JSON body"
    Right payloadVal -> do
      logInfo
        lgr
        "Processing webhook"
        [ ("name", webhookCfg.name),
          ("payload_size", T.pack (show (LBS.length body)))
        ]

      -- Apply per-endpoint model/thinking overrides
      let envWithOverrides = applyOverrides env webhookCfg

      -- Assemble prompt from configured inputs (synchronous — can fail fast)
      promptResult <- assemblePrompt envWithOverrides.workspace webhookCfg.prompt

      case promptResult of
        Nothing -> do
          logError lgr "No prompt content resolved" [("name", webhookCfg.name)]
          respond $ jsonResponse status500 $ errorJson "No prompt content resolved"
        Just assembled -> do
          let promptText = renderTemplate assembled payloadVal
          now <- getCurrentTime

          -- Fork event handling and return 202 immediately
          _ <- forkIO $ processWebhookEvent lgr webhookCfg envWithOverrides payloadVal promptText now

          respond $ jsonResponse status202 acceptedJson

-- | Process a webhook event asynchronously (runs in a forked thread)
processWebhookEvent :: Logger -> WebhookConfig -> AppEnv -> Value -> Text -> UTCTime -> IO ()
processWebhookEvent lgr webhookCfg env payloadVal promptText now =
  handle (\(e :: SomeException) -> logError lgr "Webhook processing exception" [("name", webhookCfg.name), ("error", T.pack (show e))]) $
    case webhookCfg.suppressIfContains of
      Nothing -> do
        let evt =
              Event
                { source = WebhookSource webhookCfg.name,
                  timestamp = now,
                  payload = payloadVal,
                  prompt = promptText,
                  image = Nothing,
                  session = webhookCfg.session,
                  deliveryTarget = webhookCfg.deliveryTarget
                }
        result <- handleEvent env evt
        case result of
          Right _ ->
            logInfo lgr "Webhook processed successfully" [("name", webhookCfg.name)]
          Left err ->
            logError lgr "Webhook processing failed" [("name", webhookCfg.name), ("error", err)]
      Just suppressPattern -> do
        let evt =
              Event
                { source = WebhookSource webhookCfg.name,
                  timestamp = now,
                  payload = payloadVal,
                  prompt = promptText,
                  image = Nothing,
                  session = webhookCfg.session,
                  deliveryTarget = LogOnly
                }
        buffered <- handleEventBuffered env evt webhookCfg.deliveryTarget
        case buffered of
          BufferedSuccess responseText flushAction -> do
            if suppressPattern `T.isInfixOf` responseText
              then do
                atomically $ writeTVar env.attachmentQueue []
                logInfo
                  lgr
                  "Webhook response suppressed"
                  [("name", webhookCfg.name), ("pattern", suppressPattern)]
              else do
                flushAction
                logInfo lgr "Webhook processed successfully" [("name", webhookCfg.name)]
          BufferedError err ->
            logError lgr "Webhook processing failed" [("name", webhookCfg.name), ("error", err)]

-- | Apply per-endpoint agent overrides to the environment
applyOverrides :: AppEnv -> WebhookConfig -> AppEnv
applyOverrides env wc =
  let newProfile = resolveProfile (toOverrides env.agentProfile <> wc.overrides)
   in env {agentProfile = newProfile}

-- | status400 for bad request
status400 :: Status
status400 = mkStatus 400 "Bad Request"

-- | Find a webhook by name
findWebhook :: Text -> [WebhookConfig] -> Maybe WebhookConfig
findWebhook n = find (\wc -> wc.name == n)

-- | Render a prompt template with JSON payload
--
-- Supports mustache-style {{.field}} placeholders.
-- Nested fields use dot notation: {{.user.name}}
renderTemplate :: Text -> Value -> Text
renderTemplate template payloadVal = go template
  where
    go :: Text -> Text
    go t = case T.breakOn "{{." t of
      (before, rest)
        | T.null rest -> t
        | otherwise ->
            let afterOpen = T.drop 3 rest -- Drop "{{."
             in case T.breakOn "}}" afterOpen of
                  (fieldPath, afterClose)
                    | T.null afterClose -> t -- Malformed, no closing }}
                    | otherwise ->
                        let value = lookupPath (T.splitOn "." fieldPath) payloadVal
                            remaining = T.drop 2 afterClose -- Drop "}}"
                         in before <> value <> go remaining

    lookupPath :: [Text] -> Value -> Text
    lookupPath [] v = valueToText v
    lookupPath (k : keys) (Object obj) =
      case KM.lookup (fromText k) obj of
        Just v -> lookupPath keys v
        Nothing -> ""
    lookupPath _ _ = ""

    fromText :: Text -> KM.Key
    fromText = Key.fromText

    valueToText :: Value -> Text
    valueToText (String s) = s
    valueToText (Number n) = T.pack (show n)
    valueToText (Bool True) = "true"
    valueToText (Bool False) = "false"
    valueToText Null = ""
    valueToText v = TE.decodeUtf8 $ LBS.toStrict $ encode v

-- | Create a JSON response
jsonResponse :: Status -> LBS.ByteString -> Response
jsonResponse status =
  responseLBS status [(hContentType, "application/json")]

-- | Create error JSON
errorJson :: Text -> LBS.ByteString
errorJson msg = encode $ Object $ KM.fromList [("status", String "error"), ("error", String msg)]

-- | Create accepted JSON (fire-and-forget acknowledgement)
acceptedJson :: LBS.ByteString
acceptedJson = encode $ Object $ KM.fromList [("status", String "accepted")]
