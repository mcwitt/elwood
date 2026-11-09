-- | End-to-end specification of the API's thinking-block prefix check. A fake
-- Messages API binds every thinking block it issues to the request it answered
-- and rejects, as Claude Fable 5.2 does, any later request that sends the block
-- after a changed prefix (system, tools, model or earlier messages, ignoring
-- thinking) or after a different run of earlier thinking blocks (a gap or a
-- block put back). Elwood then runs whole turns through 'handleEventWith'.
module Test.Elwood.ThinkingPrefix (tests) where

import Colog.Core (LogAction (..))
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (bracket)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Data.Vector qualified as V
import Elwood.AgentSettings qualified as Profile (AgentProfile (..))
import Elwood.AgentSettings qualified as Settings
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (newPruneHorizons)
import Elwood.Config (CompactionConfig (..), CompactionStrategy (..), PruningConfig (..), PruningStrategy (..), ToolDirectionConfig (..), ToolPruningConfig (..))
import Elwood.Event (AppEnv (..), DeliveryPolicy (..), Event (..), handleEventWith)
import Elwood.Event.Types (DeliveryTarget (..), EventSource (..), SessionConfig (..))
import Elwood.Metrics (newMetricsStore)
import Elwood.Positive (literal)
import Elwood.Prompt (PromptInput (..))
import Elwood.Provider (ApiFormat (..), ProviderConfig (..), ToolResultImageMode (..))
import Elwood.Scheduler (newCallbackStore)
import Elwood.Session (newSessionLocks)
import Elwood.Telegram.Client qualified as Telegram
import Elwood.Thinking (ThinkingMode (..))
import Elwood.Tools qualified as Tools
import Elwood.Tools.AsyncTask (closeTaskStore)
import Elwood.Tools.Registry (registerTool)
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Network.HTTP.Types.Status (Status, status200, status400)
import Network.Wai (pathInfo, requestHeaders, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Thinking block prefix check"
    [ testCase "pruning never leaves a thinking block after an edited message" $
        withPrefixApi Nothing (Just pruning) [] $ \fixture -> do
          -- Small tool traffic is never pruned, so the projection stays fixed
          -- and earlier turns keep their thinking. A big exchange is pruned at
          -- the next turn, which must drop the thinking produced before.
          mapM_ fixture.turn ["small 1", "small 2", "big 3", "small 4", "small 5"]
          fixture.verify
          readIORef fixture.api >>= \api -> do
            assertBool "some request carried thinking from an earlier turn" (api.carried > 0)
            assertBool "some request dropped thinking from an earlier turn" (api.dropped > 0),
      testCase "compaction keeps no stale thinking" $
        withPrefixApi (Just compaction) Nothing [] $ \fixture -> do
          mapM_ fixture.turn ["small 1", "small 2", "small 3", "small 4"]
          fixture.verify
          readIORef fixture.api >>= \api -> assertBool "compaction ran" (api.summaries > 0),
      testCase "a changed system prompt drops earlier thinking" $
        withPrefixApi Nothing Nothing [WorkspaceFile "SOUL.md"] $ \fixture -> do
          fixture.soul "first"
          mapM_ fixture.turn ["small 1", "small 2"]
          fixture.soul "second"
          mapM_ fixture.turn ["small 3", "small 4"]
          fixture.verify
          readIORef fixture.api >>= \api -> do
            assertBool "thinking survived turns with an unchanged prompt" (api.carried > 0)
            assertBool "thinking was dropped after the prompt changed" (api.dropped > 0),
      testCase "a reported mismatch is retried once with drop_block" $
        withPrefixApi Nothing Nothing [] $ \fixture -> do
          modifyIORef' fixture.api (\api -> api {rejectNext = True})
          fixture.turn "small 1"
          api <- readIORef fixture.api
          api.violations @?= []
          api.dropBlock @?= 2
    ]
  where
    pruning =
      PruningConfig
        { strategy = KeepTurns 1,
          thinking = Nothing,
          tools = Just (ToolPruningConfig 20 20 (KeepTurns 1) (ToolDirectionConfig (KeepTurns 1) 20 20) (ToolDirectionConfig (KeepTurns 1) 20 20))
        }
    compaction = CompactionConfig (literal @1) (Settings.ModelRef "mock" "summarizer") Nothing (CKeepTurns (literal @1))

-- | One issued thinking block: the context and thinking-free messages before
-- it, and the thinking blocks that preceded it.
data Binding = Binding {context :: Value, prefix :: [Value], thinking :: [Text]}

data Api = Api
  { issued :: Map Text Binding,
    violations :: [String],
    -- | Requests that resent thinking from an earlier turn.
    carried :: Int,
    -- | Requests that omitted thinking from an earlier turn.
    dropped :: Int,
    summaries :: Int,
    dropBlock :: Int,
    -- | Reject the next request as a prefix mismatch, whatever it contains.
    rejectNext :: Bool,
    turnSignatures :: [Text]
  }

data Fixture = Fixture {turn :: Text -> IO (), soul :: Text -> IO (), verify :: IO (), api :: IORef Api}

withPrefixApi :: Maybe CompactionConfig -> Maybe PruningConfig -> [PromptInput] -> (Fixture -> IO a) -> IO a
withPrefixApi compaction pruning prompt action = withSystemTempDirectory "thinking-prefix" $ \directory -> do
  state <- newIORef (Api Map.empty [] 0 0 0 0 False [])
  let application request respond = do
        body <- strictRequestBody request
        let beta = maybe False (BS.isInfixOf "thinking-binding-controls-2026-08-01") (lookup "anthropic-beta" (requestHeaders request))
        (status, reply) <- case (pathInfo request, decode body) of
          (["v1", "messages"], Just (Object fields)) -> atomicModifyIORef' state (answer beta fields)
          _ -> pure (status400, object [])
        respond (responseLBS status [] (encode reply))
  testWithApplication (pure application) $ \port -> do
    let logger = LogAction (const (pure ()))
        url = "http://127.0.0.1:" <> show port
        echo = Tool (Claude.ToolSchema "fetch" "Fetch text" (object [])) $ \input ->
          pure (ToolSuccess [Claude.ToolResultText (case input of Object o | Just (String q) <- KM.lookup "query" o -> q; _ -> "")])
    telegram <- Telegram.newClient logger "token"
    claude <- Claude.newClient (Map.singleton "mock" (ProviderConfig "mock" (T.pack url) Nothing AnthropicFormat ImagesEmbedded))
    conversations <- Claude.newInMemoryConversationStore
    metrics <- newMetricsStore
    horizons <- newPruneHorizons
    sessions <- newSessionLocks
    toggles <- newTVarIO Map.empty
    callbacks <- newCallbackStore logger directory
    bracket (Tools.newAsyncTaskStore 3600) closeTaskStore $ \tasks -> do
      let profile =
            (Settings.resolveProfile mempty)
              { Profile.model = Settings.ModelRef "mock" "claude-fable-5-2",
                Profile.systemPrompt = prompt,
                Profile.thinking = Just (Adaptive Nothing Nothing),
                Profile.cache = Nothing
              }
          env =
            AppEnv
              logger
              telegram {Telegram.tcBaseUrl = url}
              claude
              conversations
              (registerTool echo Tools.newToolRegistry)
              Tools.noApprovalChannel
              compaction
              pruning
              directory
              profile
              Map.empty
              metrics
              horizons
              sessions
              False
              toggles
              (Settings.AgentPreset Nothing mempty)
              Map.empty
              []
              Nothing
              tasks
              callbacks
          runTurn text = do
            modifyIORef' state (\api -> api {turnSignatures = []})
            now <- getCurrentTime
            result <- handleEventWith env Immediate (Event (WebhookSource "test") now Null text Nothing [] (Named "session") LogOnly)
            violations <- (.violations) <$> readIORef state
            assertEqual ("violations after " <> T.unpack text) [] violations
            result @?= Right "done"
      action
        Fixture
          { turn = runTurn,
            soul = T.writeFile (directory </> "SOUL.md"),
            verify = readIORef state >>= \api -> api.violations @?= [],
            api = state
          }

answer :: Bool -> KM.KeyMap Value -> Api -> (Api, (Status, Value))
answer beta fields api
  | KM.lookup "system" fields == Just (String "Summarize conversations concisely.") =
      (api {summaries = api.summaries + 1}, ok "end_turn" [text "summary"])
  | api.rejectNext = (api {rejectNext = False}, mismatch)
  | not (null problems) && not dropping = (api {violations = api.violations <> problems}, mismatch)
  | otherwise =
      let signature = "sig-" <> T.pack (show (Map.size api.issued))
          binding = Binding context (map stripThinking messages) (map fst sent)
          earlier = [s | (s, _) <- sent, s `notElem` api.turnSignatures]
          omitted = [s | s <- Map.keys api.issued, s `notElem` map fst sent, s `notElem` api.turnSignatures]
          next =
            api
              { issued = Map.insert signature binding api.issued,
                carried = api.carried + fromEnum (not (null earlier)),
                dropped = api.dropped + fromEnum (not (null omitted)),
                dropBlock = api.dropBlock + fromEnum dropping,
                turnSignatures = api.turnSignatures <> [signature]
              }
          thought = object ["type" .= ("thinking" :: Text), "thinking" .= ("reasoning" :: Text), "signature" .= signature]
       in (next, if continuing then ok "end_turn" [thought, text "done"] else ok "tool_use" [thought, call])
  where
    messages = maybe [] toList (KM.lookup "messages" fields >>= array)
    context = object ["system" .= KM.lookup "system" fields, "tools" .= KM.lookup "tools" fields, "model" .= KM.lookup "model" fields]
    dropping =
      beta && case KM.lookup "thinking" fields of
        Just (Object t) -> KM.lookup "block_binding" t == Just (object ["prefix_mismatch_behavior" .= ("drop_block" :: Text)])
        _ -> False
    -- Every thinking block sent, with the index of its message.
    sent = [(s, i) | (i, m) <- zip [0 ..] messages, s <- signatures m]
    problems = concat [check s i (map fst (takeWhile ((/= s) . fst) sent)) | (s, i) <- sent]
    check s i before = case Map.lookup s api.issued of
      Nothing -> ["unknown thinking block " <> T.unpack s]
      Just binding ->
        [T.unpack s <> ": context changed" | binding.context /= context]
          <> [T.unpack s <> ": an earlier message changed" | binding.prefix /= map stripThinking (take i messages)]
          <> [T.unpack s <> ": earlier thinking is not a suffix of " <> show binding.thinking <> ": " <> show before | not (before `isSuffixOf` binding.thinking)]
    continuing = case reverse messages of
      Object m : _ | Just (Array blocks) <- KM.lookup "content" m -> any isResult blocks
      _ -> False
    isResult (Object b) = KM.lookup "type" b == Just (String "tool_result")
    isResult _ = False
    prompt = case [t | Object m <- messages, KM.lookup "role" m == Just (String "user"), Just (Array bs) <- [KM.lookup "content" m], Object b <- toList bs, Just (String t) <- [KM.lookup "text" b]] of
      [] -> ""
      ts -> last ts
    query = if "big" `T.isPrefixOf` prompt then T.replicate 400 "x" else "q"
    call = object ["type" .= ("tool_use" :: Text), "id" .= ("call-" <> T.pack (show (Map.size api.issued))), "name" .= ("fetch" :: Text), "input" .= object ["query" .= query]]
    text t = object ["type" .= ("text" :: Text), "text" .= (t :: Text)]
    ok stop content = (status200, object ["id" .= ("reply" :: Text), "stop_reason" .= (stop :: Text), "content" .= content, "usage" .= object ["input_tokens" .= (1 :: Int), "output_tokens" .= (1 :: Int)]])
    mismatch =
      ( status400,
        object
          [ "type" .= ("error" :: Text),
            "error"
              .= object
                [ "type" .= ("invalid_request_error" :: Text),
                  "message" .= ("Invalid `signature` in `thinking` block. The block is bound to a different conversation. Remove the block, or set `thinking.block_binding.prefix_mismatch_behavior` to \"drop_block\"." :: Text)
                ]
          ]
      )
    array (Array a) = Just a
    array _ = Nothing

signatures :: Value -> [Text]
signatures (Object m) | Just (Array blocks) <- KM.lookup "content" m = [s | Object b <- toList blocks, isThinking b, Just (String s) <- [KM.lookup "signature" b]]
signatures _ = []

stripThinking :: Value -> Value
stripThinking (Object m) | Just (Array blocks) <- KM.lookup "content" m = Object (KM.insert "content" (Array (V.filter keep blocks)) m)
  where
    keep (Object b) = not (isThinking b)
    keep _ = True
stripThinking v = v

isThinking :: KM.KeyMap Value -> Bool
isThinking b = KM.lookup "type" b `elem` [Just (String "thinking"), Just (String "redacted_thinking")]
