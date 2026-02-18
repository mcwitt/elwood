module Test.Elwood.Memory (tests) where

import Data.Text qualified as T
import Elwood.Memory (sanitizeKey)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Memory"
    [ sanitizeKeyTests,
      sanitizeKeyProperties
    ]

sanitizeKeyTests :: TestTree
sanitizeKeyTests =
  testGroup
    "sanitizeKey"
    [ testCase "accepts alphanumeric" $
        sanitizeKey "hello123" @?= Just "hello123",
      testCase "accepts hyphens" $
        sanitizeKey "my-key" @?= Just "my-key",
      testCase "accepts underscores" $
        sanitizeKey "my_key" @?= Just "my_key",
      testCase "removes special characters" $
        sanitizeKey "hello!@#world" @?= Just "helloworld",
      testCase "removes spaces" $
        sanitizeKey "hello world" @?= Just "helloworld",
      testCase "removes path separators" $
        sanitizeKey "../../../etc/passwd" @?= Just "etcpasswd",
      testCase "returns Nothing for empty result" $
        sanitizeKey "!@#$%^&*()" @?= Nothing,
      testCase "returns Nothing for empty input" $
        sanitizeKey "" @?= Nothing,
      testCase "truncates long keys to 100 chars" $
        let longKey = T.replicate 150 "a"
            result = sanitizeKey longKey
         in fmap T.length result @?= Just 100,
      testCase "mixed case preserved" $
        sanitizeKey "HelloWorld" @?= Just "HelloWorld"
    ]

sanitizeKeyProperties :: TestTree
sanitizeKeyProperties =
  testGroup
    "sanitizeKey properties"
    [ testProperty "never returns empty Just" $
        \(s :: String) ->
          case sanitizeKey (T.pack s) of
            Nothing -> True
            Just t -> not (T.null t),
      testProperty "result only contains ASCII safe chars" $
        \(s :: String) ->
          -- Only test ASCII inputs since sanitizeKey allows Unicode alphanumeric
          let asciiOnly = filter (\c -> fromEnum c < 128) s
           in case sanitizeKey (T.pack asciiOnly) of
                Nothing -> True
                Just t -> T.all isSafeChar t,
      testProperty "result length <= 100" $
        \(s :: String) ->
          case sanitizeKey (T.pack s) of
            Nothing -> True
            Just t -> T.length t <= 100
    ]

-- | Check if a character is safe for filesystem keys
isSafeChar :: Char -> Bool
isSafeChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_")
