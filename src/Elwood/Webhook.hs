module Elwood.Webhook
  ( -- * Server
    runWebhookServer,
    webhookApp,

    -- * Template Rendering
    renderTemplate,

    -- * Types
    WebhookConfig (..),
    WebhookServerConfig (..),
  )
where

import Elwood.Webhook.Server
import Elwood.Webhook.Types
