-- | A tool that delivers a message to the user mid-turn, verbatim.
--
-- Text the model writes before a tool call is not reliably delivered as
-- written: depending on the model it arrives as a text block, as a short
-- progress-update thinking block, or not at all. Tool inputs are never
-- summarized, so routing must-see content through a tool guarantees it
-- reaches the user intact before the turn finishes.
module Elwood.Tools.SendMessage
  ( mkSendMessageTool,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Logging (Logger, logInfo)
import Elwood.Tools.Types

-- | Construct the @send_message@ tool. The delivery action sends to the
-- event's chats (immediately for chat turns, buffered and replayed for
-- webhook turns) and reports failure, which the tool passes on as an error so
-- the model knows the user did not see the message.
mkSendMessageTool :: Logger -> (Text -> IO (Either Text ())) -> Tool
mkSendMessageTool logger deliver =
  Tool
    { schema =
        ToolSchema
          { name = "send_message",
            description =
              "Send a message to the user now, before this turn finishes. "
                <> "Use it for anything the user must see exactly as written mid-turn: "
                <> "a deliverable, a question, a partial result, a progress update with specific numbers. "
                <> "Plain text you write before a tool call may reach the user only as a brief progress note, "
                <> "depending on the model; the text after your last tool call is always delivered as your final reply, "
                <> "so do not repeat a sent message there.",
            inputSchema = sendMessageSchema
          },
      execute = \input -> case parseInput input of
        Left err -> pure $ toolError err
        Right msg ->
          deliver msg >>= \case
            Left err -> pure $ toolError ("Delivery failed; the user has not seen this message: " <> err)
            Right () -> do
              logInfo logger "Message sent via tool" [("length", T.pack (show (T.length msg)))]
              pure $ toolSuccess "{\"status\":\"sent\"}"
    }

-- | JSON Schema for send_message input
sendMessageSchema :: Value
sendMessageSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "message"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The message to deliver, in markdown" :: Text)
                ]
          ],
      "required" .= (["message"] :: [Text])
    ]

-- | Parse send_message input
parseInput :: Value -> Either Text Text
parseInput (Aeson.Object obj) = case KM.lookup "message" obj of
  Just (Aeson.String m)
    | T.null (T.strip m) -> Left "'message' must not be empty"
    | otherwise -> Right m
  _ -> Left "Missing or invalid 'message' parameter"
parseInput _ = Left "Expected object input"
