module Test.Elwood.Provider (tests) where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Elwood.Provider (ApiFormat (..), ProviderConfigFile (..), ToolResultImageMode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Provider"
    [ testCase "parses a full provider entry" $ do
        let yaml = "{\"base_url\": \"http://host:9000\", \"api_key_env\": \"LOCAL_KEY\", \"format\": \"anthropic\"}"
        case eitherDecodeStrict (yaml :: ByteString) :: Either String ProviderConfigFile of
          Left e -> assertFailure e
          Right pcf -> do
            pcf.baseUrl @?= "http://host:9000"
            pcf.apiKeyEnv @?= Just ("LOCAL_KEY" :: Text)
            pcf.format @?= Just AnthropicFormat,
      testCase "base_url is required (missing => parse error)" $ do
        let input = "{\"api_key\": \"x\"}" :: ByteString
        case eitherDecodeStrict input :: Either String ProviderConfigFile of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected parse failure but succeeded",
      testCase "unknown format => parse error" $ do
        let input = "{\"base_url\": \"http://h\", \"format\": \"openai\"}" :: ByteString
        case eitherDecodeStrict input :: Either String ProviderConfigFile of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected parse failure but succeeded",
      testCase "unknown key => parse error" $ do
        let input = "{\"base_url\": \"http://h\", \"bogus\": 1}" :: ByteString
        case eitherDecodeStrict input :: Either String ProviderConfigFile of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected parse failure but succeeded",
      testCase "parses tool_result_images values" $ do
        let hoisted = "{\"base_url\": \"http://h\", \"tool_result_images\": \"hoisted\"}" :: ByteString
            embedded = "{\"base_url\": \"http://h\", \"tool_result_images\": \"embedded\"}" :: ByteString
            absent = "{\"base_url\": \"http://h\"}" :: ByteString
        case eitherDecodeStrict hoisted :: Either String ProviderConfigFile of
          Left e -> assertFailure e
          Right pcf -> pcf.toolResultImages @?= Just ImagesHoisted
        case eitherDecodeStrict embedded :: Either String ProviderConfigFile of
          Left e -> assertFailure e
          Right pcf -> pcf.toolResultImages @?= Just ImagesEmbedded
        case eitherDecodeStrict absent :: Either String ProviderConfigFile of
          Left e -> assertFailure e
          Right pcf -> pcf.toolResultImages @?= Nothing,
      testCase "invalid tool_result_images => parse error" $ do
        let input = "{\"base_url\": \"http://h\", \"tool_result_images\": \"sideways\"}" :: ByteString
        case eitherDecodeStrict input :: Either String ProviderConfigFile of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected parse failure but succeeded"
    ]
