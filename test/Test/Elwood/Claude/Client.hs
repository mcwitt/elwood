module Test.Elwood.Claude.Client
  ( tests,
  )
where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Elwood.Claude.Client
  ( RetryConfig (..),
    calculateRetryDelay,
    defaultRetryConfig,
    isRetryableError,
    retryWithBackoff,
  )
import Elwood.Claude.Types (ClaudeError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Elwood.Claude.Client"
    [ testGroup "isRetryableError" isRetryableTests,
      testGroup "calculateRetryDelay" calculateDelayTests,
      testGroup "retryWithBackoff" retryTests
    ]

-- | Tests for isRetryableError
isRetryableTests :: [TestTree]
isRetryableTests =
  [ testCase "rate limited is retryable" $
      assertBool "should be retryable" $
        isRetryableError (ClaudeRateLimited (Just 30)),
    testCase "rate limited without retry-after is retryable" $
      assertBool "should be retryable" $
        isRetryableError (ClaudeRateLimited Nothing),
    testCase "overloaded is retryable" $
      assertBool "should be retryable" $
        isRetryableError (ClaudeOverloaded (Just 60)),
    testCase "HTTP error is not retryable" $
      assertBool "should not be retryable" $
        not $
          isRetryableError (ClaudeHttpError 500 "error"),
    testCase "parse error is not retryable" $
      assertBool "should not be retryable" $
        not $
          isRetryableError (ClaudeParseError "bad json"),
    testCase "API error is not retryable" $
      assertBool "should not be retryable" $
        not $
          isRetryableError (ClaudeApiError "invalid_request" "bad input")
  ]

-- | Tests for calculateRetryDelay
calculateDelayTests :: [TestTree]
calculateDelayTests =
  [ testCase "uses retry-after header when present (rate limited)" $
      assertEqual
        "should use header value"
        30
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited (Just 30)) 0),
    testCase "uses retry-after header when present (overloaded)" $
      assertEqual
        "should use header value"
        45
        (calculateRetryDelay defaultRetryConfig (ClaudeOverloaded (Just 45)) 0),
    testCase "caps retry-after at maxDelay" $
      assertEqual
        "should cap at maxDelay"
        60
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited (Just 120)) 0),
    testCase "exponential backoff attempt 0" $
      assertEqual
        "should be baseDelay * 2^0 = 5"
        5
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited Nothing) 0),
    testCase "exponential backoff attempt 1" $
      assertEqual
        "should be baseDelay * 2^1 = 10"
        10
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited Nothing) 1),
    testCase "exponential backoff attempt 2" $
      assertEqual
        "should be baseDelay * 2^2 = 20"
        20
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited Nothing) 2),
    testCase "exponential backoff capped at maxDelay" $
      assertEqual
        "should cap at 60"
        60
        (calculateRetryDelay defaultRetryConfig (ClaudeRateLimited Nothing) 10),
    testCase "custom config respected" $
      let config = defaultRetryConfig {baseDelay = 2, maxDelay = 30}
       in assertEqual
            "should use custom baseDelay"
            8
            (calculateRetryDelay config (ClaudeRateLimited Nothing) 2)
  ]

-- | Tests for retryWithBackoff
retryTests :: [TestTree]
retryTests =
  [ testCase "success on first attempt returns immediately" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            modifyIORef attemptsRef (+ 1)
            pure (Right "success" :: Either ClaudeError String)
          noDelay _ = pure ()

      result <- retryWithBackoff defaultRetryConfig action noDelay

      attempts <- readIORef attemptsRef
      assertEqual "should succeed" (Right "success") result
      assertEqual "should only attempt once" 1 attempts,
    testCase "retries on rate limit then succeeds" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            n <- readIORef attemptsRef
            writeIORef attemptsRef (n + 1)
            if n < 2
              then pure (Left $ ClaudeRateLimited (Just 1))
              else pure (Right "success")
          noDelay _ = pure ()

      result <- retryWithBackoff defaultRetryConfig action noDelay

      attempts <- readIORef attemptsRef
      assertEqual "should succeed" (Right "success") result
      assertEqual "should attempt 3 times" 3 attempts,
    testCase "stops after max retries" $ do
      attemptsRef <- newIORef (0 :: Int)
      let config = defaultRetryConfig {maxRetries = 2}
          action :: IO (Either ClaudeError ())
          action = do
            modifyIORef attemptsRef (+ 1)
            pure (Left $ ClaudeRateLimited Nothing)
          noDelay _ = pure ()

      result <- retryWithBackoff config action noDelay

      attempts <- readIORef attemptsRef
      assertEqual "should fail" (Left $ ClaudeRateLimited Nothing) result
      -- 1 initial + 2 retries = 3 attempts
      assertEqual "should attempt maxRetries + 1 times" 3 attempts,
    testCase "does not retry non-retryable errors" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action :: IO (Either ClaudeError ())
          action = do
            modifyIORef attemptsRef (+ 1)
            pure (Left $ ClaudeHttpError 500 "server error")
          noDelay _ = pure ()

      result <- retryWithBackoff defaultRetryConfig action noDelay

      attempts <- readIORef attemptsRef
      assertEqual "should fail" (Left $ ClaudeHttpError 500 "server error") result
      assertEqual "should only attempt once" 1 attempts,
    testCase "calls onRetry callback" $ do
      callbackRef <- newIORef ([] :: [(Int, Int)])
      let config =
            defaultRetryConfig
              { maxRetries = 2,
                onRetry = Just $ \attempt secs _err ->
                  modifyIORef callbackRef ((attempt, secs) :)
              }
          action = pure (Left $ ClaudeRateLimited (Just 30))
          noDelay _ = pure ()

      _ <- retryWithBackoff config action noDelay

      callbacks <- readIORef callbackRef
      -- Callbacks are prepended, so reverse to get chronological order
      assertEqual "should call callback for each retry" [(2, 30), (1, 30)] callbacks,
    testCase "delay function is called with correct seconds" $ do
      delaysRef <- newIORef ([] :: [Int])
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            n <- readIORef attemptsRef
            writeIORef attemptsRef (n + 1)
            if n < 2
              then pure (Left $ ClaudeRateLimited (Just 15))
              else pure (Right "success")
          trackDelay s = modifyIORef delaysRef (s :)

      _ <- retryWithBackoff defaultRetryConfig action trackDelay

      delays <- readIORef delaysRef
      assertEqual "should delay correct seconds" [15, 15] delays
  ]
