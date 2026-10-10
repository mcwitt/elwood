module Elwood.Tools.SendMessage (mkSendMessageTool) where

import Data.Aeson (withObject)
import Data.Text (Text)
import Elwood.Logging (Logger)
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types

mkSendMessageTool :: Logger -> (Text -> IO (Either Text ())) -> Tool
mkSendMessageTool _ deliver =
  B.tool
    "send_message"
    "Send a message verbatim to the user before the turn finishes; do not repeat it in the final reply."
    (B.schema [("message", B.field "string" "Message in Markdown")] ["message"])
    (withObject "send_message" (`B.nonempty` "message"))
    (fmap result . deliver)
  where
    result = either (toolError . ("Delivery failed; the user has not seen this message: " <>)) (const (toolSuccess "{\"status\":\"sent\"}"))
