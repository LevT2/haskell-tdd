module ParserTest where

import Parser08.Parser (Token (..), lookAhead, accept, ScanError(..))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

prettyTestCaseShow :: Show a => String -> a -> TestTree
prettyTestCaseShow output construct =
  testCase output $   show construct @?= output

prettyTestCase :: (Eq b, Show b) =>
                  (String -> Either ScanError b) -> String -> Either ScanError b -> TestTree
prettyTestCase f input expected =
  case expected of
    Left error -> case error of
      BadInput msg -> testCase msg $ f input @?= Left (BadInput input)
    Right value -> testCase (if null input then "Empty input" else input) $ f input @?= Right value

testParser :: TestTree
--testParser = testGroup "tokenizer tests" [bareTest, lookAheadTest]

testParser = testGroup "tokenizer tests" [lookAheadTest, acceptTest]

bareTest :: TestTree
bareTest =
  testGroup
    "basic Tasty test"
    [   testCase "Empty input:" $ lookAhead []    @?= Right TokEnd
      , testCase "Bad input: *" $ lookAhead "*"   @?= Left (BadInput "*")
      , testCase "()"           $ lookAhead "()"  @?= Right TokLParen
    ]

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
        prettyTestCase accept []    $ Left (BadInput "Nothing to accept")
      , prettyTestCase accept ""    $ Left (BadInput "Nothing to accept")
      , prettyTestCase accept "*"   $ Right ""
      , prettyTestCase accept "(*)" $ Right "*)"
    ]
