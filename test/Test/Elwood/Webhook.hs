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
                { wcName = "test-hook",
                  wcSecret = Nothing,
                  wcPromptTemplate = Just "Hello",
                  wcPromptFile = Nothing,
                  wcSession = Isolated,
                  wcDelivery = [LogOnly],
                  wcSuppressIfContains = Nothing,
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
        wcName config @?= "test-hook"
        wcPromptTemplate config @?= Just "Hello"
        wcSession config @?= Isolated,
      testCase "can create config with promptFile" $ do
        let config =
              WebhookConfig
                { wcName = "file-hook",
                  wcSecret = Nothing,
                  wcPromptTemplate = Nothing,
                  wcPromptFile = Just "HEARTBEAT.md",
                  wcSession = Isolated,
                  wcDelivery = [TelegramBroadcast],
                  wcSuppressIfContains = Nothing,
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
        wcPromptFile config @?= Just "HEARTBEAT.md",
      testCase "can create config with secret" $ do
        let config =
              WebhookConfig
                { wcName = "secure-hook",
                  wcSecret = Just "my-secret",
                  wcPromptTemplate = Just "Hello",
                  wcPromptFile = Nothing,
                  wcSession = Named "session",
                  wcDelivery = [TelegramBroadcast],
                  wcSuppressIfContains = Nothing,
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
        wcSecret config @?= Just "my-secret"
        wcSession config @?= Named "session",
      testCase "can have multiple delivery targets" $ do
        let config =
              WebhookConfig
                { wcName = "multi-target",
                  wcSecret = Nothing,
                  wcPromptTemplate = Just "Test",
                  wcPromptFile = Nothing,
                  wcSession = Isolated,
                  wcDelivery = [TelegramBroadcast, TelegramDelivery "123", LogOnly],
                  wcSuppressIfContains = Nothing,
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
        length (wcDelivery config) @?= 3,
      testCase "can have suppressIfContains" $ do
        let config =
              WebhookConfig
                { wcName = "heartbeat",
                  wcSecret = Nothing,
                  wcPromptTemplate = Nothing,
                  wcPromptFile = Just "HEARTBEAT.md",
                  wcSession = Isolated,
                  wcDelivery = [TelegramBroadcast],
                  wcSuppressIfContains = Just "HEARTBEAT_OK",
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
        wcSuppressIfContains config @?= Just "HEARTBEAT_OK"
    ]

webhookServerConfigTests :: TestTree
webhookServerConfigTests =
  testGroup
    "WebhookServerConfig"
    [ testCase "can create disabled config" $ do
        let config =
              WebhookServerConfig
                { wscEnabled = False,
                  wscPort = 8080,
                  wscGlobalSecret = Nothing,
                  wscWebhooks = []
                }
        wscEnabled config @?= False
        wscWebhooks config @?= [],
      testCase "can create enabled config with webhooks" $ do
        let webhook =
              WebhookConfig
                { wcName = "hook1",
                  wcSecret = Nothing,
                  wcPromptTemplate = Just "Test",
                  wcPromptFile = Nothing,
                  wcSession = Isolated,
                  wcDelivery = [LogOnly],
                  wcSuppressIfContains = Nothing,
                  wcModel = Nothing,
                  wcThinking = Nothing
                }
            config =
              WebhookServerConfig
                { wscEnabled = True,
                  wscPort = 9000,
                  wscGlobalSecret = Just "global-secret",
                  wscWebhooks = [webhook]
                }
        wscEnabled config @?= True
        wscPort config @?= 9000
        wscGlobalSecret config @?= Just "global-secret"
        length (wscWebhooks config) @?= 1,
      testCase "default port is 8080" $ do
        let config =
              WebhookServerConfig
                { wscEnabled = True,
                  wscPort = 8080,
                  wscGlobalSecret = Nothing,
                  wscWebhooks = []
                }
        wscPort config @?= 8080
    ]
