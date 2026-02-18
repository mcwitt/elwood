{-# LANGUAGE StrictData #-}

module Elwood.Webhook.Server
  ( -- * Server
    runWebhookServer,
    webhookApp,

    -- * Template Rendering
    renderTemplate,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Elwood.Event (Event (..), EventEnv (..), EventSource (..), handleEvent)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
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
runWebhookServer :: WebhookServerConfig -> EventEnv -> IO ()
runWebhookServer config env = do
  let logger = eeLogger env
      port = wscPort config

  logInfo logger "Starting webhook server" [("port", T.pack (show port))]

  run port (webhookApp config env)
    `catch` \(e :: SomeException) ->
      logError logger "Webhook server error" [("error", T.pack (show e))]

-- | WAI application for webhook handling
webhookApp :: WebhookServerConfig -> EventEnv -> Application
webhookApp config env request respond = do
  let logger = eeLogger env
      path = pathInfo request

  case path of
    ["webhook", webhookName] -> do
      -- Find matching webhook config
      case findWebhook webhookName (wscWebhooks config) of
        Nothing -> do
          logWarn logger "Unknown webhook" [("name", webhookName)]
          respond $ jsonResponse status404 $ errorJson "Unknown webhook"
        Just webhookConfig -> do
          -- Verify authentication
          let effectiveSecret = wcSecret webhookConfig <|> wscGlobalSecret config
          case effectiveSecret of
            Just secret -> do
              let providedSecret = lookup webhookSecretHeader (requestHeaders request)
              if providedSecret == Just (TE.encodeUtf8 secret)
                then handleWebhookRequest logger webhookConfig env request respond
                else do
                  logWarn logger "Invalid webhook secret" [("name", webhookName)]
                  respond $ jsonResponse status401 $ errorJson "Invalid or missing secret"
            Nothing ->
              -- No secret required
              handleWebhookRequest logger webhookConfig env request respond
    ["health"] -> do
      -- Health check endpoint
      respond $ jsonResponse status200 "{\"status\": \"ok\"}"
    _ -> do
      logWarn logger "Unknown path" [("path", T.intercalate "/" path)]
      respond $ jsonResponse status404 $ errorJson "Not found"
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Handle a webhook request after authentication
handleWebhookRequest :: Logger -> WebhookConfig -> EventEnv -> Request -> (Response -> IO a) -> IO a
handleWebhookRequest logger webhookConfig env request respond = do
  -- Read and parse request body
  body <- strictRequestBody request

  case eitherDecode body of
    Left err -> do
      logWarn logger "Invalid JSON body" [("error", T.pack err)]
      respond $ jsonResponse status400 $ errorJson "Invalid JSON body"
    Right payload -> do
      logInfo
        logger
        "Processing webhook"
        [ ("name", wcName webhookConfig),
          ("payload_size", T.pack (show (LBS.length body)))
        ]

      -- Render prompt template with payload
      let prompt = renderTemplate (wcPromptTemplate webhookConfig) payload

      -- Create event
      now <- getCurrentTime
      let event =
            Event
              { evSource = WebhookSource (wcName webhookConfig),
                evTimestamp = now,
                evPayload = payload,
                evPrompt = prompt,
                evImage = Nothing,
                evSession = wcSession webhookConfig,
                evDelivery = wcDelivery webhookConfig
              }

      -- Handle the event
      result <- handleEvent env event

      case result of
        Right responseText -> do
          logInfo logger "Webhook processed successfully" [("name", wcName webhookConfig)]
          respond $ jsonResponse status200 $ successJson responseText
        Left err -> do
          logError logger "Webhook processing failed" [("name", wcName webhookConfig), ("error", err)]
          respond $ jsonResponse status500 $ errorJson err

-- | status400 for bad request
status400 :: Status
status400 = mkStatus 400 "Bad Request"

-- | Find a webhook by name
findWebhook :: Text -> [WebhookConfig] -> Maybe WebhookConfig
findWebhook name = find (\wc -> wcName wc == name)

-- | Render a prompt template with JSON payload
--
-- Supports mustache-style {{.field}} placeholders.
-- Nested fields use dot notation: {{.user.name}}
renderTemplate :: Text -> Value -> Text
renderTemplate template payload = go template
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
                        let value = lookupPath (T.splitOn "." fieldPath) payload
                            remaining = T.drop 2 afterClose -- Drop "}}"
                         in before <> value <> go remaining

    lookupPath :: [Text] -> Value -> Text
    lookupPath [] v = valueToText v
    lookupPath (key : keys) (Object obj) =
      case KM.lookup (fromText key) obj of
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
