module Test.Elwood.Webhook (tests) where

import Data.Aeson (Value (..), object, (.=))
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Webhook.Server (renderTemplate)
import Elwood.Webhook.Types
  ( WebhookConfig (..),
    WebhookServerConfig (..),
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Webhook"
    [ templateRenderingTests,
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
        renderTemplate template payload @?= "Count: 42.0",
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
    "WebhookConfig"
    [ testCase "can create config with promptTemplate" $ do
        let config =
              WebhookConfig
                { name = "test-hook",
                  secret = Nothing,
                  promptTemplate = Just "Hello",
                  session = Isolated,
                  delivery = [LogOnly],
                  suppressIfEquals = Nothing,
                  model = Nothing,
                  thinking = Nothing
                }
        config.name @?= "test-hook"
        config.promptTemplate @?= Just "Hello"
        config.session @?= Isolated,
      testCase "can create config with secret" $ do
        let config =
              WebhookConfig
                { name = "secure-hook",
                  secret = Just "my-secret",
                  promptTemplate = Just "Hello",
                  session = Named "session",
                  delivery = [TelegramBroadcast],
                  suppressIfEquals = Nothing,
                  model = Nothing,
                  thinking = Nothing
                }
        config.secret @?= Just "my-secret"
        config.session @?= Named "session",
      testCase "can have multiple delivery targets" $ do
        let config =
              WebhookConfig
                { name = "multi-target",
                  secret = Nothing,
                  promptTemplate = Just "Test",
                  session = Isolated,
                  delivery = [TelegramBroadcast, TelegramDelivery "123", LogOnly],
                  suppressIfEquals = Nothing,
                  model = Nothing,
                  thinking = Nothing
                }
        length config.delivery @?= 3,
      testCase "can have suppressIfEquals" $ do
        let config =
              WebhookConfig
                { name = "heartbeat",
                  secret = Nothing,
                  promptTemplate = Just "Check system health",
                  session = Isolated,
                  delivery = [TelegramBroadcast],
                  suppressIfEquals = Just "HEARTBEAT_OK",
                  model = Nothing,
                  thinking = Nothing
                }
        config.suppressIfEquals @?= Just "HEARTBEAT_OK"
    ]

webhookServerConfigTests :: TestTree
webhookServerConfigTests =
  testGroup
    "WebhookServerConfig"
    [ testCase "can create disabled config" $ do
        let config =
              WebhookServerConfig
                { enabled = False,
                  port = 8080,
                  globalSecret = Nothing,
                  webhooks = []
                }
        config.enabled @?= False
        config.webhooks @?= [],
      testCase "can create enabled config with webhooks" $ do
        let webhook =
              WebhookConfig
                { name = "hook1",
                  secret = Nothing,
                  promptTemplate = Just "Test",
                  session = Isolated,
                  delivery = [LogOnly],
                  suppressIfEquals = Nothing,
                  model = Nothing,
                  thinking = Nothing
                }
            config =
              WebhookServerConfig
                { enabled = True,
                  port = 9000,
                  globalSecret = Just "global-secret",
                  webhooks = [webhook]
                }
        config.enabled @?= True
        config.port @?= 9000
        config.globalSecret @?= Just "global-secret"
        length config.webhooks @?= 1,
      testCase "default port is 8080" $ do
        let config =
              WebhookServerConfig
                { enabled = True,
                  port = 8080,
                  globalSecret = Nothing,
                  webhooks = []
                }
        config.port @?= 8080
    ]
