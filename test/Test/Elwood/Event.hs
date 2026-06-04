module Test.Elwood.Event (tests) where

import Elwood.Event (attachPromptNote, sessionToConversationId)
import Elwood.Event.Types (SavedAttachment (..), SessionConfig (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Event"
    [sessionIdTests, attachPromptNoteTests]

sessionIdTests :: TestTree
sessionIdTests =
  testGroup
    "sessionToConversationId"
    [ testCase "Isolated returns Nothing" $
        sessionToConversationId Isolated @?= Nothing,
      testCase "Named returns Just with session name" $
        sessionToConversationId (Named "my-session") @?= Just "my-session",
      testCase "Named with numeric name passes through as-is" $
        sessionToConversationId (Named "12345") @?= Just "12345",
      testCase "Named sessions with different names return different IDs" $ do
        let id1 = sessionToConversationId (Named "session1")
        let id2 = sessionToConversationId (Named "session2")
        id1 /= id2 @?= True
    ]

photoAtt :: SavedAttachment
photoAtt =
  SavedAttachment {path = "inbox/1-a.jpg", mediaType = "image/jpeg", sizeBytes = 250880, perceivable = True, originalName = Nothing}

pdfAtt :: SavedAttachment
pdfAtt =
  SavedAttachment {path = "inbox/1-b.pdf", mediaType = "application/pdf", sizeBytes = 1258291, perceivable = False, originalName = Just "report.pdf"}

attachPromptNoteTests :: TestTree
attachPromptNoteTests =
  testGroup
    "attachPromptNote"
    [ testCase "empty list leaves prompt unchanged" $
        attachPromptNote "hello" [] @?= "hello",
      testCase "single perceivable attachment, empty prompt" $
        attachPromptNote "" [photoAtt]
          @?= "[System: archived 1 inbound attachment to the workspace inbox:\n\
              \- `inbox/1-a.jpg` — image/jpeg, 245 KB (also attached above for viewing)]",
      testCase "two attachments joined to a non-empty prompt" $
        attachPromptNote "save this" [photoAtt, pdfAtt]
          @?= "save this\n\n\
              \[System: archived 2 inbound attachments to the workspace inbox:\n\
              \- `inbox/1-a.jpg` — image/jpeg, 245 KB (also attached above for viewing)\n\
              \- `inbox/1-b.pdf` (original: report.pdf) — application/pdf, 1 MB (saved-only; not viewable inline)]",
      testCase "renders original filename for a saved-only document" $
        attachPromptNote "" [pdfAtt]
          @?= "[System: archived 1 inbound attachment to the workspace inbox:\n\
              \- `inbox/1-b.pdf` (original: report.pdf) — application/pdf, 1 MB (saved-only; not viewable inline)]"
    ]
