{-# LANGUAGE MultiParamTypeClasses #-}

module ParserTest where

import           Parser08.Parser

import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))


import Data.Convertible.Base (Convertible(), safeConvert)


prettyTestCaseShow :: Show a => String -> a -> TestTree
prettyTestCaseShow output construct =
  testCase output $   show construct @?= output


instance Convertible TParseError TParseError where safeConvert = Right
instance Convertible TScanError TParseError where safeConvert = Right . ScanError

prettyTestCase :: (Eq b, Show b) =>
                  (String -> Either TParseError b) -> String -> Either TParseError  b -> TestTree
prettyTestCase f input expected = case expected of
    Left error -> case error of
      UnconsumedString str -> testCase str $ f input @?= Left (UnconsumedString input)
      ScanError error' ->  case error' of
                                NothingToAccept -> testCase (input ++ ": Nothing to accept") $ f input @?= Left (ScanError NothingToAccept) --NothingToAccept
                                BadInput msg -> testCase msg $ f input @?= Left (ScanError $ BadInput input)
    Right value -> testCase (if null input then "Empty input" else input) $ f input @?= Right value

test = testGroup "parsetests" [parseTest, acceptTest]
--testParser = testGroup "tokenizer tests" [parseTest]

--testTokenizer = testGroup "tokenizer tests" [lookAheadTest, acceptTest, parseTest]


--bareTest :: TestTree
--bareTest =
--  testGroup
--    "basic Tasty test"
--    [   testCase "Empty input:" $ lookAhead []    @?= Right TokEnd
--      , testCase "Bad input: *" $ lookAhead "*"   @?= Left (BadInput "*")
--      , testCase "()"           $ lookAhead "()"  @?= Right TokLParen
--    ]

--lookAheadTest :: TestTree
--lookAheadTest =
--  testGroup
--    "lookAheadTest"
--    [
--        prettyTestCase lookAhead []   $ Right TokEnd
--      , prettyTestCase lookAhead ""   $ Right TokEnd
--      , prettyTestCase lookAhead "*"  $ Left (BadInput "*")
--      , prettyTestCase lookAhead "()" $ Right TokLParen
--    ]

accept' = fmapL ScanError . accept
fmapL f = either (Left . f) Right

acceptTest :: TestTree
acceptTest =
  testGroup
    "acceptTest"
    [
        prettyTestCase accept' []    $ Left (ScanError NothingToAccept)
      , prettyTestCase accept' ""    $ Left (ScanError NothingToAccept)
      , prettyTestCase accept' "*"   $ Right ""
      , prettyTestCase accept' "(*)" $ Right "*)"
    ]

parseTest :: TestTree
parseTest =
  testGroup "parseTest"
  [
      prettyTestCase parse "()" $ Right Leaf
    , prettyTestCase parse "*"  $ Left (UnconsumedString "*")
  ]
