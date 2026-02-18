{-# LANGUAGE StrictData #-}

module Elwood.Claude.Client
  ( ClaudeClient (..),
    newClaudeClient,
    sendMessages,
    sendMessagesWithRetry,
    RetryConfig (..),
    defaultRetryConfig,

    -- * Exported for testing
    isRetryableError,
    calculateRetryDelay,
    retryWithBackoff,
  )
where

import Control.Concurrent (threadDelay)
import Data.Aeson (eitherDecode, encode, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)

-- | Claude API client
data ClaudeClient = ClaudeClient
  { -- | HTTP connection manager
    ccManager :: Manager,
    -- | Anthropic API key
    ccApiKey :: Text,
    -- | Base URL for API calls
    ccBaseUrl :: String
  }

-- | Retry configuration for API calls
data RetryConfig = RetryConfig
  { -- | Maximum number of retry attempts
    rcMaxRetries :: Int,
    -- | Base delay for exponential backoff (seconds) when no retry-after header
    rcBaseDelay :: Int,
    -- | Maximum delay cap (seconds)
    rcMaxDelay :: Int,
    -- | Callback for retry notifications (retry number, wait seconds, error)
    rcOnRetry :: Maybe (Int -> Int -> ClaudeError -> IO ())
  }

-- | Default retry configuration
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { rcMaxRetries = 3,
      rcBaseDelay = 5,
      rcMaxDelay = 60,
      rcOnRetry = Nothing
    }

-- | Create a new Claude client
newClaudeClient :: Text -> IO ClaudeClient
newClaudeClient apiKey = do
  manager <- newManager tlsManagerSettings
  pure
    ClaudeClient
      { ccManager = manager,
        ccApiKey = apiKey,
        ccBaseUrl = "https://api.anthropic.com"
      }

-- | Send a messages request to Claude (single attempt, no retry)
sendMessages :: ClaudeClient -> MessagesRequest -> IO (Either ClaudeError MessagesResponse)
sendMessages client req = do
  httpReq <- buildRequest client
  let body = encode req
      httpReq' =
        httpReq
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs httpReq' (ccManager client)
  let status = statusCode $ responseStatus response
      respBody = responseBody response
      retryAfter = parseRetryAfter response

  pure $ parseResponse status respBody retryAfter

-- | Check if an error is retryable (rate limits and overload)
isRetryableError :: ClaudeError -> Bool
isRetryableError (ClaudeRateLimited _) = True
isRetryableError (ClaudeOverloaded _) = True
isRetryableError _ = False

-- | Calculate retry delay based on error and attempt number
--
-- Uses retry-after header if available, otherwise exponential backoff
calculateRetryDelay :: RetryConfig -> ClaudeError -> Int -> Int
calculateRetryDelay config err attempt =
  case err of
    ClaudeRateLimited (Just secs) -> min secs (rcMaxDelay config)
    ClaudeOverloaded (Just secs) -> min secs (rcMaxDelay config)
    _ ->
      -- Exponential backoff: baseDelay * 2^attempt, capped at maxDelay
      min (rcBaseDelay config * (2 ^ attempt)) (rcMaxDelay config)

-- | Generic retry combinator with configurable delay function
--
-- Takes the action and delay function as parameters for testability
retryWithBackoff ::
  RetryConfig ->
  -- | Action to retry
  IO (Either ClaudeError a) ->
  -- | Delay function (seconds -> IO ())
  (Int -> IO ()) ->
  IO (Either ClaudeError a)
retryWithBackoff config action delayFn = go 0
  where
    go attempt = do
      result <- action
      case result of
        Right resp -> pure (Right resp)
        Left err
          | isRetryableError err && attempt < rcMaxRetries config -> do
              let waitSeconds = calculateRetryDelay config err attempt
              -- Notify about retry if callback is configured
              case rcOnRetry config of
                Just notify -> notify (attempt + 1) waitSeconds err
                Nothing -> pure ()
              -- Wait and retry
              delayFn waitSeconds
              go (attempt + 1)
          | otherwise -> pure (Left err)

-- | Send a messages request with automatic retry on rate limits
sendMessagesWithRetry ::
  ClaudeClient ->
  RetryConfig ->
  MessagesRequest ->
  IO (Either ClaudeError MessagesResponse)
sendMessagesWithRetry client config req =
  retryWithBackoff config (sendMessages client req) delaySeconds
  where
    delaySeconds s = threadDelay (s * 1000000)

-- | Parse the retry-after header from response
parseRetryAfter :: Response a -> Maybe Int
parseRetryAfter response =
  lookup hRetryAfter (responseHeaders response)
    >>= readMaybe . BS8.unpack

-- | Build the HTTP request with proper headers
buildRequest :: ClaudeClient -> IO Request
buildRequest client = do
  req <- parseRequest $ ccBaseUrl client <> "/v1/messages"
  pure
    req
      { requestHeaders =
          [ ("Content-Type", "application/json"),
            ("x-api-key", TE.encodeUtf8 $ ccApiKey client),
            ("anthropic-version", "2023-06-01")
          ]
      }

-- | Parse the API response, handling errors appropriately
parseResponse :: Int -> ByteString -> Maybe Int -> Either ClaudeError MessagesResponse
parseResponse status body retryAfter
  | status == 200 =
      case eitherDecode body of
        Left err -> Left $ ClaudeParseError err
        Right resp -> Right resp
  | status == 429 =
      Left $ ClaudeRateLimited retryAfter
  | status == 529 =
      Left $ ClaudeOverloaded retryAfter
  | otherwise =
      Left $ parseApiError status body

-- | Parse an API error response
parseApiError :: Int -> ByteString -> ClaudeError
parseApiError status body =
  case Aeson.decode body of
    Just obj -> case parseErrorFromJson obj of
      Just (errType, errMsg) -> ClaudeApiError errType errMsg
      Nothing -> ClaudeHttpError status (decodeBody body)
    Nothing -> ClaudeHttpError status (decodeBody body)
  where
    decodeBody :: ByteString -> Text
    decodeBody = TE.decodeUtf8 . LBS.toStrict

    parseErrorFromJson :: Aeson.Value -> Maybe (Text, Text)
    parseErrorFromJson = parseMaybe $ withObject "ErrorResponse" $ \v -> do
      errObj <- v .: "error"
      (,) <$> errObj .: "type" <*> errObj .: "message"
