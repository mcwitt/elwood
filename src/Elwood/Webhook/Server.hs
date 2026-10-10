module Elwood.Webhook.Server (WebhookHandle (..), runWebhookServer, webhookApp, renderTemplate) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Elwood.Event (DeliveryPolicy (..), Event (..), Runner (..))
import Elwood.Event.Types
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logError)
import Elwood.Prompt (assemblePrompt)
import Elwood.Webhook.Types
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

data WebhookHandle = WebhookHandle
  { runner :: Runner,
    workspace :: FilePath,
    metrics :: IO LBS.ByteString,
    submit :: Maybe Text -> IO () -> IO (),
    logger :: Logger
  }

runWebhookServer :: WebhookServerConfig -> WebhookHandle -> IO ()
runWebhookServer config handle = run config.port (webhookApp config handle)

webhookApp :: WebhookServerConfig -> WebhookHandle -> Application
webhookApp config handle request respond = case pathInfo request of
  ["health"] -> json status200 ["status" .= ("ok" :: Text)]
  ["metrics"] -> handle.metrics >>= respond . responseLBS status200 [(hContentType, "text/plain; version=0.0.4; charset=utf-8")]
  ["webhook", name] -> case find ((== name) . (.name)) config.webhooks of
    Nothing -> failure status404 "Unknown webhook"
    Just endpoint
      | maybe False (\secret -> lookup "X-Webhook-Secret" (requestHeaders request) /= Just (TE.encodeUtf8 secret)) config.secret ->
          failure status401 "Invalid or missing secret"
      | otherwise -> do
          body <- strictRequestBody request
          case eitherDecode body of
            Left _ -> failure status400 "Invalid JSON body"
            Right payload -> do
              assembled <- assemblePrompt handle.workspace endpoint.prompt
              case assembled of
                Nothing -> failure status500 "No prompt content resolved"
                Just template -> do
                  now <- getCurrentTime
                  let event =
                        Event
                          (WebhookSource endpoint.name)
                          now
                          payload
                          (renderTemplate template payload)
                          Nothing
                          []
                          endpoint.session
                          endpoint.deliveryTarget
                      policy = maybe Immediate SuppressContaining endpoint.suppressIfContains
                      lane = case endpoint.session of Named sid -> Just sid; Isolated -> Nothing
                  handle.submit lane $
                    catchSync
                      (handle.runner.run endpoint.overrides policy event >>= either (report . T.unpack) (const (pure ())))
                      (report . show)
                  json status202 ["status" .= ("accepted" :: Text)]
  _ -> failure status404 "Not found"
  where
    json status = respond . responseLBS status [(hContentType, "application/json")] . encode . object
    failure status message = json status ["status" .= ("error" :: Text), "error" .= (message :: Text)]
    report err = logError handle.logger "Webhook processing failed" [("error", T.pack err)]

-- Template interpolation is a left-to-right fold; substituted text is never
-- interpreted as another template. Missing paths denote empty text.
renderTemplate :: Text -> Value -> Text
renderTemplate template payload = go template
  where
    go text = case T.breakOn "{{." text of
      (_, rest) | T.null rest -> text
      (before, rest) -> case T.breakOn "}}" (T.drop 3 rest) of
        (_, end) | T.null end -> text
        (path, end) -> before <> render (lookupPath (T.splitOn "." path) payload) <> go (T.drop 2 end)
    lookupPath [] value = value
    lookupPath (key : keys) (Object fields) = maybe Null (lookupPath keys) (KM.lookup (Key.fromText key) fields)
    lookupPath _ _ = Null
    render (String text) = text
    render Null = ""
    render value = TE.decodeUtf8 (LBS.toStrict (encode value))
