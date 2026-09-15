module Test.Elwood.Tools.SendMessage (tests) where

import Colog.Core (LogAction (..))
import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolResultPart (..), ToolSchema (..))
import Elwood.Tools.SendMessage (mkSendMessageTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.SendMessage"
    [ testCase "delivers the message through the callback" $ do
        sent <- newIORef ([] :: [Text])
        let tool = mkSendMessageTool (LogAction (const (pure ()))) (\m -> Right <$> modifyIORef' sent (<> [m]))
        result <- tool.execute (object ["message" .= ("Here is your plan:\n\n1. Rest" :: Text)])
        result @?= ToolSuccess [ToolResultText "{\"status\":\"sent\"}"]
        delivered <- readIORef sent
        delivered @?= ["Here is your plan:\n\n1. Rest"],
      testCase "rejects a missing message" $ do
        sent <- newIORef ([] :: [Text])
        let tool = mkSendMessageTool (LogAction (const (pure ()))) (\m -> Right <$> modifyIORef' sent (<> [m]))
        result <- tool.execute (object ["text" .= ("oops" :: Text)])
        case result of
          ToolError _ -> pure ()
          other -> assertFailure $ "expected ToolError, got: " <> show other
        delivered <- readIORef sent
        delivered @?= [],
      testCase "rejects a blank message" $ do
        let tool = mkSendMessageTool (LogAction (const (pure ()))) (\_ -> assertFailure "should not deliver")
        result <- tool.execute (object ["message" .= ("  \n" :: Text)])
        case result of
          ToolError _ -> pure ()
          other -> assertFailure $ "expected ToolError, got: " <> show other,
      testCase "reports a failed delivery as a tool error" $ do
        let tool = mkSendMessageTool (LogAction (const (pure ()))) (\_ -> pure (Left "connection refused"))
        result <- tool.execute (object ["message" .= ("Hello" :: Text)])
        case result of
          ToolError err -> assertBool "mentions the cause" ("connection refused" `T.isInfixOf` err)
          other -> assertFailure $ "expected ToolError, got: " <> show other,
      testCase "is named send_message" $ do
        let tool = mkSendMessageTool (LogAction (const (pure ()))) (\_ -> pure (Right ()))
        tool.schema.name @?= "send_message"
    ]
