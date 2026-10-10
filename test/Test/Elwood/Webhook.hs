module Test.Elwood.Webhook (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent.MVar
import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.Types (Pair)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Last (..))
import Elwood.AgentSettings (AgentOverrides (..), ModelRefOverrides (..))
import Elwood.Event (DeliveryPolicy (..), Event (..), Runner (..))
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Prompt (PromptInput (..))
import Elwood.Webhook.Server (WebhookHandle (..), renderTemplate, webhookApp)
import Elwood.Webhook.Types
  ( WebhookConfig (..),
    WebhookServerConfig (..),
  )
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Webhook"
    [ httpTests,
      templateRenderingTests,
      webhookConfigTests,
      webhookServerConfigTests
    ]

templateRenderingTests :: TestTree
templateRenderingTests =
  testGroup
    "renderTemplate"
    [ testCase "renders simple field" $ do
        let template = "Hello {{.name}}!"
            payload = object ["name" .= ("World" :: String)]
        renderTemplate template payload @?= "Hello World!",
      testCase "renders multiple fields" $ do
        let template = "{{.greeting}} {{.name}}!"
            payload = object ["greeting" .= ("Hi" :: String), "name" .= ("Alice" :: String)]
        renderTemplate template payload @?= "Hi Alice!",
      testCase "handles missing field gracefully" $ do
        let template = "Hello {{.name}}!"
            payload = object []
        renderTemplate template payload @?= "Hello !",
      testCase "renders nested field" $ do
        let template = "User: {{.user.name}}"
            payload = object ["user" .= object ["name" .= ("Bob" :: String)]]
        renderTemplate template payload @?= "User: Bob",
      testCase "renders number field" $ do
        let template = "Count: {{.count}}"
            payload = object ["count" .= (42 :: Int)]
        renderTemplate template payload @?= "Count: 42",
      testCase "renders boolean true" $ do
        let template = "Active: {{.active}}"
            payload = object ["active" .= True]
        renderTemplate template payload @?= "Active: true",
      testCase "renders boolean false" $ do
        let template = "Active: {{.active}}"
            payload = object ["active" .= False]
        renderTemplate template payload @?= "Active: false",
      testCase "handles null field" $ do
        let template = "Value: {{.value}}"
            payload = object ["value" .= Null]
        renderTemplate template payload @?= "Value: ",
      testCase "preserves text without placeholders" $ do
        let template = "No placeholders here"
            payload = object ["unused" .= ("value" :: String)]
        renderTemplate template payload @?= "No placeholders here",
      testCase "handles malformed placeholder (no closing)" $ do
        let template = "Hello {{.name"
            payload = object ["name" .= ("World" :: String)]
        -- Should return template as-is when malformed
        renderTemplate template payload @?= "Hello {{.name",
      testCase "renders deeply nested field" $ do
        let template = "City: {{.address.city.name}}"
            payload =
              object
                [ "address"
                    .= object
                      [ "city"
                          .= object
                            ["name" .= ("NYC" :: String)]
                      ]
                ]
        renderTemplate template payload @?= "City: NYC",
      testCase "handles non-object for nested path" $ do
        let template = "Value: {{.foo.bar}}"
            payload = object ["foo" .= ("string" :: String)]
        renderTemplate template payload @?= "Value: ",
      testCase "renders multiline template" $ do
        let template = "Line 1: {{.a}}\nLine 2: {{.b}}"
            payload = object ["a" .= ("first" :: String), "b" .= ("second" :: String)]
        renderTemplate template payload @?= "Line 1: first\nLine 2: second"
    ]

webhookConfigTests :: TestTree
webhookConfigTests =
  testGroup
    "Webhook endpoint parsing"
    [ testCase "missing fields resolve endpoint defaults" $
        endpoint [] @?= Success (WebhookConfig "hook" [] Isolated TelegramBroadcast Nothing mempty),
      testCase "null optional endpoint fields inherit defaults" $
        endpoint ["prompt" .= Null, "session" .= Null, "delivery_target" .= Null, "agent" .= Null]
          @?= endpoint [],
      testCase "telegram delivery preserves every destination" $
        case endpoint ["delivery_target" .= target "telegram" ["chat_ids" .= ([123, 456, 789] :: [Int])]] of
          Success cfg -> cfg.deliveryTarget @?= TelegramDelivery (123 :| [456, 789])
          Error err -> assertFailure err,
      testCase "broadcast delivery parses" $
        case endpoint ["delivery_target" .= target "telegram_broadcast" []] of
          Success cfg -> cfg.deliveryTarget @?= TelegramBroadcast
          Error err -> assertFailure err,
      testCase "log-only delivery parses case-insensitively" $
        case endpoint ["delivery_target" .= target "LOG_ONLY" []] of
          Success cfg -> cfg.deliveryTarget @?= LogOnly
          Error err -> assertFailure err,
      testCase "endpoint prompt, named session, suppression and overrides survive parsing" $
        case endpoint
          [ "prompt" .= [object ["type" .= ("text" :: String), "content" .= ("Hello" :: String)]],
            "session" .= ("main" :: String),
            "suppress_if_contains" .= ("quiet" :: String),
            "agent" .= object ["model" .= ("worker" :: String)]
          ] of
          Success cfg -> do
            cfg.prompt @?= [InlineText "Hello"]
            cfg.session @?= Named "main"
            cfg.suppressIfContains @?= Just "quiet"
            cfg.overrides.model.model @?= Last (Just "worker")
          Error err -> assertFailure err,
      rejects "empty telegram destination is rejected" ["delivery_target" .= target "telegram" ["chat_ids" .= ([] :: [Int])]],
      rejects "unknown endpoint fields are rejected" ["bogus" .= True],
      rejects "unknown delivery fields are rejected" ["delivery_target" .= target "telegram_broadcast" ["chat_ids" .= ([123] :: [Int])]],
      rejects "unknown delivery types are rejected" ["delivery_target" .= target "email" []]
    ]
  where
    endpoint :: [Pair] -> Result WebhookConfig
    endpoint fields = fromJSON (object ("name" .= ("hook" :: String) : fields))
    target :: String -> [Pair] -> Value
    target kind fields = object ("type" .= kind : fields)
    rejects name fields = testCase name $ case endpoint fields of
      Error _ -> pure ()
      Success _ -> assertFailure "accepted invalid endpoint"

webhookServerConfigTests :: TestTree
webhookServerConfigTests =
  testGroup
    "Webhook server parsing"
    [ testCase "empty object resolves server defaults" $
        fromJSON @WebhookServerConfig (object []) @?= Success (WebhookServerConfig False 8080 Nothing []),
      testCase "null fields inherit server defaults" $
        fromJSON @WebhookServerConfig (object ["enabled" .= Null, "port" .= Null, "secret" .= Null, "endpoints" .= Null])
          @?= Success (WebhookServerConfig False 8080 Nothing []),
      testCase "enabled server parses its secret, port and endpoints" $
        fromJSON @WebhookServerConfig
          ( object
              [ "enabled" .= True,
                "port" .= (9000 :: Int),
                "secret" .= ("secret" :: String),
                "endpoints" .= [object ["name" .= ("hook" :: String)]]
              ]
          )
          @?= Success (WebhookServerConfig True 9000 (Just "secret") [WebhookConfig "hook" [] Isolated TelegramBroadcast Nothing mempty]),
      testCase "unknown server fields are rejected" $
        case fromJSON @WebhookServerConfig (object ["endpointz" .= ([] :: [Value])]) of
          Error _ -> pure ()
          Success _ -> assertFailure "accepted unknown server key"
    ]

httpTests :: TestTree
httpTests =
  testGroup
    "HTTP contract"
    [ testCase "authentication precedes admission" $ do
        admitted <- newIORef False
        let submit _ _ = writeIORef admitted True
        withWebhook submit (Runner (\_ _ _ -> pure (Right "unused"))) $ \request manager -> do
          response <- HTTP.httpLbs request {HTTP.requestHeaders = []} manager
          statusCode (HTTP.responseStatus response) @?= 401
          readIORef admitted >>= (@?= False),
      testCase "202 acknowledges queued work, retaining destination under suppression" $ do
        pending <- newEmptyMVar
        seen <- newEmptyMVar
        let submit lane action = putMVar pending (lane, action)
            runner = Runner $ \_ policy event -> putMVar seen (policy, event.deliveryTarget, event.prompt) >> pure (Right "OK")
        withWebhook submit runner $ \request manager -> do
          response <- HTTP.httpLbs request manager
          statusCode (HTTP.responseStatus response) @?= 202
          isEmptyMVar seen >>= (@?= True)
          (lane, action) <- takeMVar pending
          lane @?= Just "session"
          action
          takeMVar seen >>= (@?= (SuppressContaining "OK", TelegramDelivery (123 :| []), "Count: 42")),
      testCase "invalid JSON is rejected before admission" $ do
        admitted <- newIORef False
        withWebhook (\_ _ -> writeIORef admitted True) (Runner (\_ _ _ -> pure (Right "unused"))) $ \request manager -> do
          response <- HTTP.httpLbs request {HTTP.requestBody = HTTP.RequestBodyLBS "invalid"} manager
          statusCode (HTTP.responseStatus response) @?= 400
          readIORef admitted >>= (@?= False)
    ]
  where
    withWebhook submit runner action = do
      let endpoint = WebhookConfig "hook" [InlineText "Count: {{.count}}"] (Named "session") (TelegramDelivery (123 :| [])) (Just "OK") mempty
          config = WebhookServerConfig True 0 (Just "secret") [endpoint]
          handle = WebhookHandle runner "." (pure "") submit (LogAction (const (pure ())))
      testWithApplication (pure (webhookApp config handle)) $ \port -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/webhook/hook")
        action request {HTTP.method = "POST", HTTP.requestHeaders = [("X-Webhook-Secret", "secret")], HTTP.requestBody = HTTP.RequestBodyLBS "{\"count\":42}"} manager
