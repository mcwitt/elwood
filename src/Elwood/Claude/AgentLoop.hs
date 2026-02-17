{-# LANGUAGE StrictData #-}

module Elwood.Claude.AgentLoop
  ( runAgentTurn
  , AgentResult (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Elwood.Claude.Client (ClaudeClient, sendMessages)
import Elwood.Claude.Types
import Elwood.Logging (Logger, logInfo, logWarn, logError)
import Elwood.Tools.Registry (ToolRegistry, lookupTool, toolSchemas)
import Elwood.Tools.Types (Tool (..), ToolEnv, ToolResult (..))

-- | Result of running an agent turn
data AgentResult
  = AgentSuccess Text [ClaudeMessage]
  -- ^ Success with response text and all messages (for persistence)
  | AgentError Text
  -- ^ Error that should be shown to user
  deriving stock (Show)

-- | Maximum iterations to prevent infinite loops
maxIterations :: Int
maxIterations = 10

-- | Run a complete agent turn, handling tool use loops
runAgentTurn
  :: Logger
  -> ClaudeClient
  -> ToolRegistry
  -> ToolEnv
  -> Maybe Text
  -- ^ System prompt
  -> Text
  -- ^ Model name
  -> [ClaudeMessage]
  -- ^ Existing conversation history
  -> ClaudeMessage
  -- ^ New user message
  -> IO AgentResult
runAgentTurn logger client registry toolEnv systemPrompt model history userMessage = do
  let messages = history ++ [userMessage]
  agentLoop logger client registry toolEnv systemPrompt model messages 0

-- | The main agent loop
agentLoop
  :: Logger
  -> ClaudeClient
  -> ToolRegistry
  -> ToolEnv
  -> Maybe Text
  -> Text
  -> [ClaudeMessage]
  -> Int
  -> IO AgentResult
agentLoop logger client registry toolEnv systemPrompt model messages iteration
  | iteration >= maxIterations = do
      logError logger "Agent loop exceeded max iterations" []
      pure $ AgentError "I've been thinking in circles. Let me try a different approach."
  | otherwise = do
      -- Build and send request
      let request =
            MessagesRequest
              { mrModel = model
              , mrMaxTokens = 4096
              , mrSystem = systemPrompt
              , mrMessages = messages
              , mrTools = toolSchemas registry
              }

      logInfo
        logger
        "Sending request to Claude"
        [ ("iteration", T.pack (show iteration))
        , ("message_count", T.pack (show (length messages)))
        ]

      result <- sendMessages client request

      case result of
        Left err -> do
          logError logger "Claude API error" [("error", T.pack (show err))]
          pure $ AgentError $ formatError err
        Right response -> do
          logInfo
            logger
            "Claude response received"
            [ ("stop_reason", maybe "none" id (mresStopReason response))
            , ("content_blocks", T.pack (show (length (mresContent response))))
            ]

          handleResponse logger client registry toolEnv systemPrompt model messages response iteration

-- | Handle Claude's response
handleResponse
  :: Logger
  -> ClaudeClient
  -> ToolRegistry
  -> ToolEnv
  -> Maybe Text
  -> Text
  -> [ClaudeMessage]
  -> MessagesResponse
  -> Int
  -> IO AgentResult
handleResponse logger client registry toolEnv systemPrompt model messages response iteration =
  case mresStopReason response of
    Just "end_turn" -> do
      -- Normal completion - extract text and return
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      pure $ AgentSuccess responseText allMessages

    Just "tool_use" -> do
      -- Tool use requested - execute tools and continue
      let toolUses = extractToolUses (mresContent response)

      logInfo
        logger
        "Tool use requested"
        [("tool_count", T.pack (show (length toolUses)))]

      -- Execute all tool uses
      toolResults <- mapM (executeToolUse logger registry toolEnv) toolUses

      -- Build messages for next iteration
      let assistantMsg = ClaudeMessage Assistant (mresContent response)
          resultBlocks = zipWith makeResultBlock toolUses toolResults
          userMsg = ClaudeMessage User resultBlocks
          newMessages = messages ++ [assistantMsg, userMsg]

      -- Continue the loop
      agentLoop logger client registry toolEnv systemPrompt model newMessages (iteration + 1)

    Just "max_tokens" -> do
      -- Hit token limit - return what we have
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      logWarn logger "Response hit max tokens" []
      pure $ AgentSuccess (responseText <> "\n\n(Response was truncated due to length)") allMessages

    other -> do
      -- Unknown stop reason
      logWarn logger "Unknown stop reason" [("reason", maybe "null" id other)]
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      pure $ AgentSuccess responseText allMessages

-- | Extract text content from content blocks
extractTextContent :: [ContentBlock] -> Text
extractTextContent blocks =
  T.intercalate "\n" [t | TextBlock t <- blocks]

-- | Extract tool use blocks
extractToolUses :: [ContentBlock] -> [ContentBlock]
extractToolUses = filter isToolUse
  where
    isToolUse (ToolUseBlock {}) = True
    isToolUse _ = False

-- | Execute a single tool use
executeToolUse :: Logger -> ToolRegistry -> ToolEnv -> ContentBlock -> IO ToolResult
executeToolUse logger registry toolEnv (ToolUseBlock tid name input) = do
  logInfo logger "Executing tool" [("tool", name), ("id", tid)]

  case lookupTool name registry of
    Nothing -> do
      logWarn logger "Unknown tool" [("tool", name)]
      pure $ ToolError $ "Unknown tool: " <> name

    Just tool -> do
      result <- toolExecute tool toolEnv input
      case result of
        ToolSuccess output ->
          logInfo logger "Tool succeeded" [("tool", name), ("output_length", T.pack (show (T.length output)))]
        ToolError err ->
          logWarn logger "Tool failed" [("tool", name), ("error", err)]
      pure result
executeToolUse _ _ _ _ = pure $ ToolError "Invalid tool use block"

-- | Make a tool result block from a tool use and its result
makeResultBlock :: ContentBlock -> ToolResult -> ContentBlock
makeResultBlock (ToolUseBlock tid _ _) result =
  case result of
    ToolSuccess output ->
      ToolResultBlock tid output False
    ToolError err ->
      ToolResultBlock tid err True
makeResultBlock _ _ =
  ToolResultBlock "" "Invalid tool use" True

-- | Format an error for user display
formatError :: ClaudeError -> Text
formatError ClaudeRateLimited =
  "I'm being rate limited right now. Please try again in a moment."
formatError ClaudeOverloaded =
  "Claude is currently overloaded. Please try again in a few minutes."
formatError (ClaudeApiError errType errMsg) =
  "Sorry, I encountered an error: " <> errType <> " - " <> errMsg
formatError (ClaudeHttpError status _) =
  "Sorry, there was a connection error (HTTP " <> T.pack (show status) <> "). Please try again."
formatError (ClaudeParseError _) =
  "Sorry, I received an unexpected response. Please try again."
