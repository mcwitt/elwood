-- | The turn algebra. Wire replies enter through 'decide'; exchanges can only
-- be completed by traversing their calls, so result order and cardinality agree.
module Elwood.Claude.Turn
  ( ToolCall (..),
    Pending,
    Decision (..),
    Outcome (..),
    Progress,
    begin,
    messages,
    callsMade,
    decide,
    pendingCalls,
    pendingMessage,
    complete,
    append,
    outputText,
    finish,
  )
where

import Data.Aeson (Object, Value (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types
import Elwood.Tools.Types (ToolResult (..))

data ToolCall = ToolCall {id :: ToolUseId, name :: ToolName, input :: Object}
  deriving stock (Show, Eq)

data Pending = Pending ClaudeMessage (NonEmpty ToolCall)
  deriving stock (Show, Eq)

data Exchange = Exchange ClaudeMessage (NonEmpty (ToolCall, ToolResult))
  deriving stock (Show, Eq)

data Decision = Finish Outcome | Continue Pending
  deriving stock (Show, Eq)

-- Provider completions; cancellation, exhaustion and failure belong to the interpreter.
data Outcome = Completed ClaudeMessage | Truncated ClaudeMessage
  deriving stock (Show, Eq)

data Progress = Progress ClaudeMessage [Exchange]
  deriving stock (Show, Eq)

data Turn = Turn Progress Outcome

begin :: ClaudeMessage -> Progress
begin input = Progress input []

pendingCalls :: Pending -> NonEmpty ToolCall
pendingCalls (Pending _ calls) = calls

pendingMessage :: Pending -> ClaudeMessage
pendingMessage (Pending message _) = message

-- | Unknown wire blocks remain available for replay, never executable.
decide :: StopReason -> [ContentBlock] -> Either Text Decision
decide reason blocks = case reason of
  MaxTokens -> Right (Finish (Truncated (ClaudeMessage Assistant (filter completeBlock blocks))))
  EndTurn | null calls -> Right (Finish (Completed message))
  EndTurn -> Left "end_turn contains client tool calls"
  ToolUse -> do
    parsed <- traverse parseCall calls
    case NE.nonEmpty parsed of
      Nothing -> Left "tool_use contains no client tool calls"
      Just batch
        | Set.size (Set.fromList (map (.id) parsed)) /= length parsed -> Left "duplicate tool call IDs"
        | otherwise -> Right (Continue (Pending message batch))
  StopReasonOther reasonText -> Left ("unsupported stop reason: " <> reasonText)
  where
    completeBlock (ToolUseBlock {}) = False
    completeBlock _ = True
    message = ClaudeMessage Assistant blocks
    calls = [(ident, name, input) | ToolUseBlock ident name input <- blocks]
    parseCall (ident, name, Object input)
      | not (T.null ident.unToolUseId || T.null name.unToolName) = Right (ToolCall ident name input)
    parseCall _ = Left "tool calls require nonempty IDs/names and object inputs"

complete :: (Applicative f) => (ToolCall -> f ToolResult) -> Pending -> f Exchange
complete execute (Pending message calls) = Exchange message <$> traverse (\call -> (call,) <$> execute call) calls

append :: Exchange -> Progress -> Progress
append exchange (Progress input exchanges) = Progress input (exchanges <> [exchange])

messages :: Progress -> [ClaudeMessage]
messages (Progress input exchanges) = input : concatMap project exchanges
  where
    project (Exchange message pairs) = [message, ClaudeMessage User (map result (toList pairs))]
    result (call, ToolSuccess parts) = ToolResultBlock call.id parts False
    result (call, ToolError err) = ToolResultBlock call.id [ToolResultText err] True

callsMade :: Progress -> Int
callsMade (Progress _ exchanges) = sum [length pairs | Exchange _ pairs <- exchanges]

-- | Rendering is a fold of the ordered exchange trace, followed by its outcome.
finish :: Progress -> Outcome -> [ClaudeMessage]
finish progress outcome = project (Turn progress outcome)
  where
    project (Turn state terminal) =
      messages state <> case terminal of
        Completed message -> [message]
        Truncated message -> [message]

outputText :: Bool -> ClaudeMessage -> Text
outputText progress message = T.intercalate "\n" [text | block <- message.content, Just text <- [visible block]]
  where
    visible (TextBlock text) = Just text
    -- The provider uses this placeholder for an interrupted progress update;
    -- it is not user narration. The terminal outcome carries truncation instead.
    visible (ThinkingBlock text _)
      | progress && not (T.null text) && text /= "This part of the response was interrupted before it finished." = Just text
    visible _ = Nothing
