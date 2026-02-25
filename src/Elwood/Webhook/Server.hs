{-# LANGUAGE StrictData #-}

module Elwood.Webhook.Server
  ( -- * Server
    runWebhookServer,
    webhookApp,

    -- * Template Rendering
    renderTemplate,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Elwood.Event (AppEnv (..), DeliveryTarget (..), Event (..), EventSource (..), deliverToTargets, handleEvent)
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
          let effectiveSecret = webhookCfg.secret <|> config.globalSecret
          case effectiveSecret of
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
      body <- renderMetrics env.metrics env.conversations env.registry env.mcpServerCount
      respond $ responseLBS status200 [("Content-Type", "text/plain; version=0.0.4; charset=utf-8")] body
    _ -> do
      logWarn lgr "Unknown path" [("path", T.intercalate "/" path)]
      respond $ jsonResponse status404 $ errorJson "Not found"

-- | Handle a webhook request after authentication
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

      -- Assemble prompt from configured inputs
      promptResult <- assemblePrompt envWithOverrides.workspaceDir webhookCfg.prompt

      case promptResult of
        Nothing -> do
          logError lgr "No prompt content resolved" [("name", webhookCfg.name)]
          respond $ jsonResponse status500 $ errorJson "No prompt content resolved"
        Just assembled -> do
          let promptText = renderTemplate assembled payloadVal
          -- Create event with LogOnly delivery - we handle notification manually
          -- to support conditional suppression
          now <- getCurrentTime
          let evt =
                Event
                  { source = WebhookSource webhookCfg.name,
                    timestamp = now,
                    payload = payloadVal,
                    prompt = promptText,
                    image = Nothing,
                    session = webhookCfg.session,
                    delivery = [LogOnly]
                  }

          -- Handle the event
          result <- handleEvent envWithOverrides evt

          case result of
            Right responseText -> do
              -- Check if we should suppress notification
              let shouldSuppress = case webhookCfg.suppressIfContains of
                    Just suppressPattern -> suppressPattern `T.isInfixOf` responseText
                    Nothing -> False

              if shouldSuppress
                then do
                  logInfo
                    lgr
                    "Webhook response suppressed"
                    [("name", webhookCfg.name), ("pattern", fromMaybe "" webhookCfg.suppressIfContains)]
                  respond $ jsonResponse status200 $ successJson responseText
                else do
                  -- Deliver to configured targets
                  deliverToTargets envWithOverrides webhookCfg.delivery responseText
                  logInfo lgr "Webhook processed successfully" [("name", webhookCfg.name)]
                  respond $ jsonResponse status200 $ successJson responseText
            Left err -> do
              logError lgr "Webhook processing failed" [("name", webhookCfg.name), ("error", err)]
              respond $ jsonResponse status500 $ errorJson err

-- | Apply per-endpoint model/thinking overrides to the environment
applyOverrides :: AppEnv -> WebhookConfig -> AppEnv
applyOverrides env wc =
  AppEnv
    { logger = env.logger,
      telegram = env.telegram,
      claude = env.claude,
      conversations = env.conversations,
      registry = env.registry,
      agentContext = env.agentContext,
      compaction = env.compaction,
      systemPromptInputs = env.systemPromptInputs,
      workspaceDir = env.workspaceDir,
      model = fromMaybe env.model wc.model,
      thinking = fromMaybe env.thinking wc.thinking,
      notifyChatIds = env.notifyChatIds,
      attachmentQueue = env.attachmentQueue,
      maxIterations = env.maxIterations,
      metrics = env.metrics,
      mcpServerCount = env.mcpServerCount,
      toolSearch = env.toolSearch,
      pruneHorizons = env.pruneHorizons
    }

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

-- | Create success JSON with response
successJson :: Text -> LBS.ByteString
successJson response = encode $ Object $ KM.fromList [("status", String "accepted"), ("response", String response)]
