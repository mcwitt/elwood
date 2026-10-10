module Elwood.Claude.Client
  ( ClaudeClient (..),
    newClient,
    sendMessages,
    RetryConfig (..),
    defaultRetryConfig,
    buildRequest,
    hoistToolResultImages,
    requestBetas,
    isRetryableError,
    calculateRetryDelay,
    retryWithBackoff,
  )
where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecode, encode, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Elwood.Claude.Types
import Elwood.Claude.Types qualified as Request (MessagesRequest (..))
import Elwood.Provider (ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Thinking (ThinkingDisplay (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)

data ClaudeClient = ClaudeClient {manager :: Manager, providers :: Map Text ProviderConfig}

data RetryConfig = RetryConfig
  {maxRetries :: Int, baseDelay :: Int, maxDelay :: Int, onRetry :: Maybe (Int -> Int -> ClaudeError -> IO ())}

defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig 3 5 60 Nothing

newClient :: Map Text ProviderConfig -> IO ClaudeClient
newClient providers = (`ClaudeClient` providers) <$> newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 600000000}

sendMessages :: ClaudeClient -> Text -> MessagesRequest -> IO (Either ClaudeError MessagesResponse)
sendMessages client providerName request@MessagesRequest {} = case Map.lookup providerName client.providers of
  Nothing -> pure (Left (ClaudeUnknownProvider providerName))
  Just provider -> do
    response <- try @HttpException $ do
      base <- buildRequest provider
      let adapted = case provider.toolResultImages of
            ImagesEmbedded -> request
            ImagesHoisted -> request {Request.messages = hoistToolResultImages request.messages}
          betas = requestBetas request
      httpLbs
        base
          { method = "POST",
            requestBody = RequestBodyLBS (encode adapted),
            requestHeaders = requestHeaders base <> [("anthropic-beta", TE.encodeUtf8 (T.intercalate "," betas)) | not (null betas)]
          }
        client.manager
    pure $ case response of
      Left err -> Left (ClaudeHttpError 0 (T.pack (show err)))
      Right reply ->
        decodeResponse
          (statusCode (responseStatus reply))
          (responseBody reply)
          (lookup hRetryAfter (responseHeaders reply) >>= readMaybe . BS.unpack)

buildRequest :: ProviderConfig -> IO Request
buildRequest provider = do
  request <- parseRequest (T.unpack (T.dropWhileEnd (== '/') provider.baseUrl) <> "/v1/messages")
  pure
    request
      { requestHeaders =
          [("Content-Type", "application/json"), ("anthropic-version", "2023-06-01")]
            <> [("x-api-key", TE.encodeUtf8 key) | Just key <- [provider.apiKey]]
      }

requestBetas :: MessagesRequest -> [Text]
requestBetas request =
  ["extended-cache-ttl-2025-04-11" | Just CacheTtl1Hour <- [request.cacheControl]]
    <> ["thinking-display-updates-2026-08-18" | Just (ThinkingConfigAdaptive _ (Just DisplayUpdates)) <- [request.thinking]]

decodeResponse :: Int -> LBS.ByteString -> Maybe Int -> Either ClaudeError MessagesResponse
decodeResponse status body retryAfter = case status of
  200 -> first ClaudeParseError (eitherDecode body)
  429 -> Left (ClaudeRateLimited retryAfter)
  529 -> Left (ClaudeOverloaded retryAfter)
  _ -> Left $ case eitherDecode @Value body >>= maybe (Left "Invalid API error") Right . parseMaybe errorParser of
    Right (kind, message) -> ClaudeApiError kind message
    Left _ -> ClaudeHttpError status (TE.decodeUtf8With lenientDecode (LBS.toStrict body))
  where
    errorParser = withObject "API error" $ \v -> v .: "error" >>= \e -> (,) <$> e .: "type" <*> e .: "message"

isRetryableError :: ClaudeError -> Bool
isRetryableError (ClaudeRateLimited _) = True
isRetryableError (ClaudeOverloaded _) = True
isRetryableError _ = False

calculateRetryDelay :: RetryConfig -> ClaudeError -> Int -> Int
calculateRetryDelay config err attempt = fromInteger (max 0 (min (toInteger config.maxDelay) seconds))
  where
    seconds = case err of
      ClaudeRateLimited (Just value) -> toInteger value
      ClaudeOverloaded (Just value) -> toInteger value
      _ -> toInteger config.baseDelay * 2 ^ max 0 (min 63 attempt)

retryWithBackoff :: RetryConfig -> IO (Either ClaudeError a) -> (Int -> IO ()) -> IO (Either ClaudeError a)
retryWithBackoff config action delay = go 0
  where
    go attempt =
      action >>= \case
        Left err | isRetryableError err && attempt < config.maxRetries -> do
          let seconds = calculateRetryDelay config err attempt
          mapM_ (\notify -> notify (attempt + 1) seconds err) config.onRetry
          delay seconds
          go (attempt + 1)
        result -> pure result

-- Hoisting is a wire projection; canonical conversation state never changes.
hoistToolResultImages :: [ClaudeMessage] -> [ClaudeMessage]
hoistToolResultImages = map $ \case
  ClaudeMessage User blocks -> let pairs = map hoist blocks in ClaudeMessage User (map fst pairs <> concatMap snd pairs)
  message -> message
  where
    hoist block@(ToolResultBlock ident parts isError)
      | images@(_ : _) <- [ImageBlock media bytes | ToolResultImage media bytes <- parts] =
          (ToolResultBlock ident (map placeholder parts) isError, TextBlock ("[Image from tool result " <> ident.unToolUseId <> ":]") : images)
      | otherwise = (block, [])
    hoist block = (block, [])
    placeholder (ToolResultImage {}) = ToolResultText "[image attached below]"
    placeholder part = part
