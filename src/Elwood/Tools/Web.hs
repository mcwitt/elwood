{-# LANGUAGE StrictData #-}

module Elwood.Tools.Web
  ( webSearchTool
  , webFetchTool
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)
import Text.HTML.TagSoup (parseTags, Tag(..), innerText)

import Elwood.Logging (logInfo, logWarn)
import Elwood.Tools.Types

-- | Tool for web search via Brave Search API
webSearchTool :: Tool
webSearchTool =
  Tool
    { toolName = "web_search"
    , toolDescription =
        "Search the web using Brave Search. "
          <> "Returns search results with titles, URLs, and descriptions."
    , toolInputSchema = webSearchSchema
    , toolExecute = executeWebSearch
    }

-- | Tool for fetching web pages
webFetchTool :: Tool
webFetchTool =
  Tool
    { toolName = "web_fetch"
    , toolDescription =
        "Fetch a web page and extract its text content. "
          <> "Useful for reading articles, documentation, etc."
    , toolInputSchema = webFetchSchema
    , toolExecute = executeWebFetch
    }

-- | JSON Schema for web_search input
webSearchSchema :: Value
webSearchSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "query"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Search query" :: Text)
                ]
          , "count"
              .= object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Number of results (default 5, max 10)" :: Text)
                ]
          ]
    , "required" .= (["query"] :: [Text])
    ]

-- | JSON Schema for web_fetch input
webFetchSchema :: Value
webFetchSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "url"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("URL to fetch" :: Text)
                ]
          ]
    , "required" .= (["url"] :: [Text])
    ]

-- | Execute web_search
executeWebSearch :: ToolEnv -> Value -> IO ToolResult
executeWebSearch env input = do
  case parseSearchInput input of
    Left err -> pure $ toolError err
    Right (query, count) -> do
      case teBraveApiKey env of
        Nothing -> do
          logWarn (teLogger env) "Web search unavailable" [("reason", "No Brave API key")]
          pure $ toolError "Web search is not configured (missing BRAVE_SEARCH_API_KEY)"
        Just apiKey -> do
          logInfo (teLogger env) "Searching web" [("query", query)]
          performSearch (teHttpManager env) apiKey query count

-- | Execute web_fetch
executeWebFetch :: ToolEnv -> Value -> IO ToolResult
executeWebFetch env input = do
  case parseFetchInput input of
    Left err -> pure $ toolError err
    Right url -> do
      logInfo (teLogger env) "Fetching URL" [("url", url)]
      fetchAndExtract (teHttpManager env) url

-- | Parse web_search input
parseSearchInput :: Value -> Either Text (Text, Int)
parseSearchInput (Aeson.Object obj) = do
  query <- case KM.lookup "query" obj of
    Just (Aeson.String q) -> Right q
    _ -> Left "Missing or invalid 'query' parameter"
  let count = case KM.lookup "count" obj of
        Just (Aeson.Number n) -> min 10 (max 1 (round n))
        _ -> 5
  Right (query, count)
parseSearchInput _ = Left "Expected object input"

-- | Parse web_fetch input
parseFetchInput :: Value -> Either Text Text
parseFetchInput (Aeson.Object obj) =
  case KM.lookup "url" obj of
    Just (Aeson.String u) -> Right u
    _ -> Left "Missing or invalid 'url' parameter"
parseFetchInput _ = Left "Expected object input"

-- | Perform Brave search
performSearch :: Manager -> Text -> Text -> Int -> IO ToolResult
performSearch manager apiKey query count = do
  result <- try $ do
    let url = "https://api.search.brave.com/res/v1/web/search?q=" <> T.unpack (urlEncode query) <> "&count=" <> show count
    req <- parseRequest url
    let req' =
          req
            { requestHeaders =
                [ ("Accept", "application/json")
                , ("X-Subscription-Token", TE.encodeUtf8 apiKey)
                ]
            }
    response <- httpLbs req' manager
    let status = statusCode $ responseStatus response
        body = responseBody response
    pure (status, body)

  case result of
    Left (e :: SomeException) ->
      pure $ toolError $ "Search request failed: " <> T.pack (show e)
    Right (status, body)
      | status == 200 -> parseSearchResults body
      | otherwise ->
          pure $
            toolError $
              "Search API returned status " <> T.pack (show status)

-- | Parse Brave search results
parseSearchResults :: LBS.ByteString -> IO ToolResult
parseSearchResults body =
  case Aeson.decode body of
    Nothing -> pure $ toolError "Failed to parse search results"
    Just val -> pure $ toolSuccess $ formatSearchResults val

-- | Format search results for display
formatSearchResults :: Value -> Text
formatSearchResults val =
  case parseMaybe parseResults val of
    Nothing -> "No results found"
    Just results -> T.intercalate "\n\n" results
  where
    parseResults = Aeson.withObject "SearchResponse" $ \obj -> do
      webObj <- obj .:? "web"
      case webObj of
        Nothing -> pure []
        Just web -> do
          results <- web .: "results"
          mapM parseResult results

    parseResult = Aeson.withObject "SearchResult" $ \obj -> do
      title <- obj .: "title"
      url <- obj .: "url"
      desc <- obj .:? "description" .!= ("" :: Text)
      pure $ "**" <> title <> "**\n" <> url <> "\n" <> desc

-- | Fetch URL and extract text
fetchAndExtract :: Manager -> Text -> IO ToolResult
fetchAndExtract manager url = do
  result <- try $ do
    req <- parseRequest (T.unpack url)
    let req' =
          req
            { requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; Elwood/1.0)")
                , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                ]
            }
    response <- httpLbs req' manager
    let status = statusCode $ responseStatus response
        body = responseBody response
    pure (status, body)

  case result of
    Left (e :: SomeException) ->
      pure $ toolError $ "Fetch failed: " <> T.pack (show e)
    Right (status, body)
      | status == 200 -> pure $ toolSuccess $ extractText body
      | otherwise ->
          pure $
            toolError $
              "Fetch returned status " <> T.pack (show status)

-- | Extract text from HTML
extractText :: LBS.ByteString -> Text
extractText body =
  let htmlText = TE.decodeUtf8With (\_ _ -> Just '?') (LBS.toStrict body)
      tags = parseTags htmlText
      -- Remove script and style content
      cleanTags = removeScriptStyle tags
      -- Extract text content
      textContent = innerText cleanTags
      -- Clean up whitespace
      cleaned = cleanWhitespace textContent
      -- Limit size
      limited = limitText 50000 cleaned
   in limited

-- | Remove script and style tags and their contents
removeScriptStyle :: [Tag Text] -> [Tag Text]
removeScriptStyle = go False
  where
    go _ [] = []
    go _ (TagOpen name _ : rest)
      | name `elem` ["script", "style"] = go True rest
    go _ (TagClose name : rest)
      | name `elem` ["script", "style"] = go False rest
    go True (_ : rest) = go True rest
    go False (t : rest) = t : go False rest

-- | Clean up excessive whitespace
cleanWhitespace :: Text -> Text
cleanWhitespace = T.unwords . T.words

-- | Limit text length
limitText :: Int -> Text -> Text
limitText maxLen t
  | T.length t <= maxLen = t
  | otherwise = T.take maxLen t <> "... (content truncated)"

-- | URL encode a query string
urlEncode :: Text -> Text
urlEncode = T.concatMap encodeChar
  where
    encodeChar c
      | c `elem` (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_.~") = T.singleton c
      | c == ' ' = "+"
      | otherwise =
          let bytes = TE.encodeUtf8 (T.singleton c)
           in T.pack $ concatMap (\b -> "%" ++ showHex b) (LBS.unpack $ LBS.fromStrict bytes)

    showHex b =
      let (h, l) = b `divMod` 16
       in [hexDigit h, hexDigit l]

    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'A' + fromIntegral n - 10)
