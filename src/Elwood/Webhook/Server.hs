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
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Elwood.Event (AppEnv (..), DeliveryTarget (..), Event (..), EventSource (..), deliverToTargets, handleEvent)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Tools.Types (ToolEnv (..))
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
import System.FilePath ((</>))

-- | Header name for webhook secret
webhookSecretHeader :: HeaderName
webhookSecretHeader = "X-Webhook-Secret"

-- | Start the webhook server
runWebhookServer :: WebhookServerConfig -> AppEnv -> IO ()
runWebhookServer config env = do
  let logger = eeLogger env
      port = wscPort config

  logInfo logger "Starting webhook server" [("port", T.pack (show port))]

  run port (webhookApp config env)
    `catch` \(e :: SomeException) ->
      logError logger "Webhook server error" [("error", T.pack (show e))]

-- | WAI application for webhook handling
webhookApp :: WebhookServerConfig -> AppEnv -> Application
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

-- | Handle a webhook request after authentication
handleWebhookRequest :: Logger -> WebhookConfig -> AppEnv -> Request -> (Response -> IO a) -> IO a
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

      -- Get prompt: either from template or from file
      promptResult <- case (wcPromptTemplate webhookConfig, wcPromptFile webhookConfig) of
        (Just template, _) -> pure $ Right $ renderTemplate template payload
        (Nothing, Just filePath) -> do
          let workspaceDir = teWorkspaceDir (eeToolEnv env)
              fullPath = workspaceDir </> filePath
          readPromptFile fullPath
        (Nothing, Nothing) -> pure $ Left "No prompt or promptFile configured"

      case promptResult of
        Left err -> do
          logError logger "Failed to get prompt" [("error", T.pack err)]
          respond $ jsonResponse status500 $ errorJson (T.pack err)
        Right prompt -> do
          -- Create event with LogOnly delivery - we handle notification manually
          -- to support conditional suppression
          now <- getCurrentTime
          let event =
                Event
                  { evSource = WebhookSource (wcName webhookConfig),
                    evTimestamp = now,
                    evPayload = payload,
                    evPrompt = prompt,
                    evImage = Nothing,
                    evSession = wcSession webhookConfig,
                    evDelivery = [LogOnly]
                  }

          -- Handle the event
          result <- handleEvent env event

          case result of
            Right responseText -> do
              -- Check if we should suppress notification
              let shouldSuppress = case wcSuppressIfContains webhookConfig of
                    Just suppressPattern -> suppressPattern `T.isInfixOf` responseText
                    Nothing -> False

              if shouldSuppress
                then do
                  logInfo
                    logger
                    "Webhook response suppressed"
                    [("name", wcName webhookConfig), ("pattern", fromMaybe "" (wcSuppressIfContains webhookConfig))]
                  respond $ jsonResponse status200 $ successJson responseText
                else do
                  -- Deliver to configured targets
                  deliverToTargets env (wcDelivery webhookConfig) responseText
                  logInfo logger "Webhook processed successfully" [("name", wcName webhookConfig)]
                  respond $ jsonResponse status200 $ successJson responseText
            Left err -> do
              logError logger "Webhook processing failed" [("name", wcName webhookConfig), ("error", err)]
              respond $ jsonResponse status500 $ errorJson err

-- | Read prompt from a file
readPromptFile :: FilePath -> IO (Either String Text)
readPromptFile path = do
  result <-
    (Right <$> TIO.readFile path)
      `catch` \(e :: SomeException) ->
        pure $ Left $ "Failed to read " <> path <> ": " <> show e
  pure $ case result of
    Left err -> Left err
    Right content
      | T.null content -> Left $ "Empty prompt file: " <> path
      | otherwise -> Right content

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
