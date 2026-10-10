module Test.Elwood.Memory (tests) where

import Data.Text qualified as T
import Elwood.Memory (parseKey)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Memory"
    [ parseKeyTests,
      parseKeyProperties
    ]

parseKeyTests :: TestTree
parseKeyTests =
  testGroup
    "parseKey"
    [ testCase "accepts alphanumeric" $
        parseKey "hello123" @?= Just "hello123",
      testCase "accepts hyphens" $
        parseKey "my-key" @?= Just "my-key",
      testCase "accepts underscores" $
        parseKey "my_key" @?= Just "my_key",
      testCase "rejects special characters" $
        parseKey "hello!@#world" @?= Nothing,
      testCase "rejects spaces" $
        parseKey "hello world" @?= Nothing,
      testCase "rejects path separators" $
        parseKey "../../../etc/passwd" @?= Nothing,
      testCase "rejects punctuation-only input" $
        parseKey "!@#$%^&*()" @?= Nothing,
      testCase "returns Nothing for empty input" $
        parseKey "" @?= Nothing,
      testCase "rejects keys longer than 100 chars" $
        let longKey = T.replicate 150 "a"
            result = parseKey longKey
         in result @?= Nothing,
      testCase "rejects keys exceeding filename byte limit" $
        parseKey (T.replicate 100 "界") @?= Nothing,
      testCase "mixed case preserved" $
        parseKey "HelloWorld" @?= Just "HelloWorld"
    ]

parseKeyProperties :: TestTree
parseKeyProperties =
  testGroup
    "parseKey properties"
    [ testProperty "never returns empty Just" $
        \(s :: String) ->
          case parseKey (T.pack s) of
            Nothing -> True
            Just t -> not (T.null t),
      testProperty "result only contains ASCII safe chars" $
        \(s :: String) ->
          -- Only test ASCII inputs since parseKey allows Unicode alphanumeric
          let asciiOnly = filter (\c -> fromEnum c < 128) s
           in case parseKey (T.pack asciiOnly) of
                Nothing -> True
                Just t -> T.all isSafeChar t,
      testProperty "result length <= 100" $
        \(s :: String) ->
          case parseKey (T.pack s) of
            Nothing -> True
            Just t -> T.length t <= 100
    ]

-- | Check if a character is safe for filesystem keys
isSafeChar :: Char -> Bool
isSafeChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_")
