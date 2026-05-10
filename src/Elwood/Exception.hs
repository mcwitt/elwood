module Elwood.Exception
  ( catchSync,
  )
where

import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)

-- | Like 'catch', but only catches synchronous exceptions. Async exceptions
-- (e.g. 'System.Timeout.Timeout') are re-thrown so the surrounding
-- machinery — timeouts, cancellation — still fires.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler =
  action `catch` \e ->
    case fromException e of
      Just (_ :: SomeAsyncException) -> throwIO e
      Nothing -> handler e
