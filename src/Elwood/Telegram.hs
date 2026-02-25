module Elwood.Telegram
  ( -- * Client
    TelegramClient,
    newClient,
    getUpdatesAllowed,
    sendMessage,
    sendMessageWithKeyboard,
    answerCallbackQuery,
    editMessageReplyMarkup,
    notify,
    getFile,
    downloadFile,
    sendPhoto,
    sendDocument,
    sendChatAction,

    -- * Polling
    runPolling,

    -- * Types
    Update (..),
    Message (..),
    Chat (..),
    User (..),
    PhotoSize (..),
    TelegramFile (..),
    CallbackQuery (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
  )
where

import Elwood.Telegram.Client
import Elwood.Telegram.Polling (runPolling)
import Elwood.Telegram.Types
