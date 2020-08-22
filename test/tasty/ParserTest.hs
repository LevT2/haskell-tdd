module ParserTest where

import           Parser08.Parser

import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

prettyTestCaseShow :: Show a => String -> a -> TestTree
prettyTestCaseShow output construct =
  testCase output $   show construct @?= output

prettyTestCase :: (Eq b, Show b) =>
                  (String -> Either ScanError b) -> String -> Either ScanError  b -> TestTree
prettyTestCase f input expected =
  case expected of
    Left error -> case error of
      NothingToAccept -> testCase (input ++ ": Nothing to accept") $ f input @?= Left NothingToAccept
      BadInput msg -> testCase msg $ f input @?= Left (BadInput input)
    Right value -> testCase (if null input then "Empty input" else input) $ f input @?= Right value

prettyTestCase2 :: (Eq b, Show b) =>
                  (String -> Either ParseError b) -> String -> Either ParseError  b -> TestTree
prettyTestCase2 f input expected =
  case expected of
    Left error -> case error of
      UnconsumedString str -> testCase str $ f input @?= Left (UnconsumedString input)
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

lookAheadTest :: TestTree
lookAheadTest =
  testGroup
    "lookAheadTest"
    [
        prettyTestCase lookAhead []   $ Right TokEnd
      , prettyTestCase lookAhead ""   $ Right TokEnd
      , prettyTestCase lookAhead "*"  $ Left (BadInput "*")
      , prettyTestCase lookAhead "()" $ Right TokLParen
    ]

acceptTest :: TestTree
acceptTest =
  testGroup
    "acceptTest"
    [
        prettyTestCase accept []    $ Left NothingToAccept
      , prettyTestCase accept ""    $ Left NothingToAccept
      , prettyTestCase accept "*"   $ Right ""
      , prettyTestCase accept "(*)" $ Right "*)"
    ]

parseTest :: TestTree
parseTest =
  testGroup "parseTest"
  [
      prettyTestCase2 parse "()" $ Right Leaf
    , prettyTestCase2 parse "*"  $ Left (UnconsumedString "*")
  ]
