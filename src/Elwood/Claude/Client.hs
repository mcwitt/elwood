module Elwood.Claude.Client
  ( ClaudeClient (..),
    newClient,
    sendMessages,
    sendMessagesWithRetry,
    RetryConfig (..),
    defaultRetryConfig,

    -- * Exported for testing
    buildRequest,
    hoistToolResultImages,
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)

-- | Claude API client: shared HTTP manager + the configured provider endpoints.
data ClaudeClient = ClaudeClient
  { manager :: Manager,
    providers :: Map Text ProviderConfig
  }

-- | Retry configuration for API calls
data RetryConfig = RetryConfig
  { -- | Maximum number of retry attempts
    maxRetries :: Int,
    -- | Base delay for exponential backoff (seconds) when no retry-after header
    baseDelay :: Int,
    -- | Maximum delay cap (seconds)
    maxDelay :: Int,
    -- | Callback for retry notifications (retry number, wait seconds, error)
    onRetry :: Maybe (Int -> Int -> ClaudeError -> IO ())
  }

-- | Default retry configuration
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { maxRetries = 3,
      baseDelay = 5,
      maxDelay = 60,
      onRetry = Nothing
    }

-- | Create a new client from the resolved provider map.
newClient :: Map Text ProviderConfig -> IO ClaudeClient
newClient provs = do
  let settings = tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro (10 * 60 * 1000000)}
  mgr <- newManager settings
  pure ClaudeClient {manager = mgr, providers = provs}

-- | Send a messages request to a named provider (single attempt, no retry).
sendMessages :: ClaudeClient -> Text -> MessagesRequest -> IO (Either ClaudeError MessagesResponse)
sendMessages client providerName req =
  case Map.lookup providerName client.providers of
    Nothing -> pure (Left (ClaudeUnknownProvider providerName))
    Just provider -> do
      httpReq <- buildRequest provider
      let body = encode (adaptForProvider provider req)
          betaHeaders = case req.cacheControl of
            Just CacheTtl1Hour -> [("anthropic-beta", "extended-cache-ttl-2025-04-11")]
            _ -> []
          httpReq' =
            httpReq
              { method = "POST",
                requestBody = RequestBodyLBS body,
                requestHeaders = requestHeaders httpReq ++ betaHeaders
              }
      response <- httpLbs httpReq' client.manager
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
    ClaudeRateLimited (Just secs) -> min secs config.maxDelay
    ClaudeOverloaded (Just secs) -> min secs config.maxDelay
    _ ->
      -- Exponential backoff: baseDelay * 2^attempt, capped at maxDelay
      min (config.baseDelay * (2 ^ attempt)) config.maxDelay

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
          | isRetryableError err && attempt < config.maxRetries -> do
              let waitSeconds = calculateRetryDelay config err attempt
              -- Notify about retry if callback is configured
              case config.onRetry of
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
  Text ->
  MessagesRequest ->
  IO (Either ClaudeError MessagesResponse)
sendMessagesWithRetry client config providerName req =
  retryWithBackoff config (sendMessages client providerName req) delaySeconds
  where
    delaySeconds s = threadDelay (s * 1000000)

-- | Parse the retry-after header from response
parseRetryAfter :: Response a -> Maybe Int
parseRetryAfter response =
  lookup hRetryAfter (responseHeaders response)
    >>= readMaybe . BS8.unpack

-- | Adapt the wire shape of a request to the provider's quirks. The
-- conversation history keeps the canonical (Anthropic) representation;
-- only the encoded request differs per provider.
adaptForProvider :: ProviderConfig -> MessagesRequest -> MessagesRequest
adaptForProvider provider req = case provider.toolResultImages of
  ImagesEmbedded -> req
  ImagesHoisted ->
    MessagesRequest
      { model = req.model,
        maxTokens = req.maxTokens,
        system = req.system,
        messages = hoistToolResultImages req.messages,
        tools = req.tools,
        thinking = req.thinking,
        cacheControl = req.cacheControl,
        toolSearch = req.toolSearch,
        outputFormat = req.outputFormat
      }

-- | Rewrite messages for endpoints whose anthropic-compat layer drops
-- image blocks inside @tool_result@ content (e.g. llama.cpp): each image
-- part is replaced with a placeholder text part, and the image is
-- re-attached as a user-message image block after the tool results,
-- labeled with the originating tool_use_id.
hoistToolResultImages :: [ClaudeMessage] -> [ClaudeMessage]
hoistToolResultImages = map hoistMessage
  where
    hoistMessage (ClaudeMessage User blocks) =
      let hoisted = map hoistBlock blocks
       in ClaudeMessage User (map fst hoisted ++ concatMap snd hoisted)
    hoistMessage msg = msg

    hoistBlock :: ContentBlock -> (ContentBlock, [ContentBlock])
    hoistBlock (ToolResultBlock tid parts isErr)
      | imgs@(_ : _) <- [(mt, d) | ToolResultImage mt d <- parts] =
          ( ToolResultBlock tid (map placeholder parts) isErr,
            TextBlock ("[Image from tool result " <> tid.unToolUseId <> ":]")
              : [ImageBlock mt d | (mt, d) <- imgs]
          )
    hoistBlock block = (block, [])

    placeholder (ToolResultImage _ _) = ToolResultText "[image attached below]"
    placeholder part = part

-- | Build the HTTP request for a provider (URL + auth headers by format).
buildRequest :: ProviderConfig -> IO Request
buildRequest provider = do
  req <- parseRequest $ T.unpack (T.dropWhileEnd (== '/') provider.baseUrl) <> "/v1/messages"
  let headers = case provider.format of
        AnthropicFormat ->
          ("Content-Type", "application/json")
            : ("anthropic-version", "2023-06-01")
            : [("x-api-key", TE.encodeUtf8 k) | Just k <- [provider.apiKey]]
  pure req {requestHeaders = headers}

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
