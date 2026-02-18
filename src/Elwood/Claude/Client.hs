{-# LANGUAGE StrictData #-}

module Elwood.Claude.Client
  ( ClaudeClient (..),
    newClaudeClient,
    sendMessages,
  )
where

import Data.Aeson (eitherDecode, encode, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Claude API client
data ClaudeClient = ClaudeClient
  { -- | HTTP connection manager
    ccManager :: Manager,
    -- | Anthropic API key
    ccApiKey :: Text,
    -- | Base URL for API calls
    ccBaseUrl :: String
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

-- | Send a messages request to Claude
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

  pure $ parseResponse status respBody

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
parseResponse :: Int -> ByteString -> Either ClaudeError MessagesResponse
parseResponse status body
  | status == 200 =
      case eitherDecode body of
        Left err -> Left $ ClaudeParseError err
        Right resp -> Right resp
  | status == 429 =
      Left ClaudeRateLimited
  | status == 529 =
      Left ClaudeOverloaded
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
