module ParserTest where

import Parser08.Parser (Token (..), lookAhead, accept)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

prettyTestCaseShow :: Show a => String -> a -> TestTree
prettyTestCaseShow output construct =
  testCase output $   show construct @?= output

prettyTestCase :: (Eq b, Show b) =>
                  (String -> Either String b) -> String -> Either String b -> TestTree
prettyTestCase f input expected =
  case expected of
    Left  msg   ->  testCase msg $                                            f input @?= Left msg
    Right value ->  testCase (if null input then "Empty input" else input) $  f input @?= Right value

testParser :: TestTree
testParser = testGroup "tokenizer tests" [lookAheadTest, acceptTest]

lookAheadTest :: TestTree
lookAheadTest =
  testGroup
    "lookAheadTest"
    [
        prettyTestCase lookAhead []   $ Right TokEnd
      , prettyTestCase lookAhead ""   $ Right TokEnd
      , prettyTestCase lookAhead "*"  $ Left "Bad input: *"
      , prettyTestCase lookAhead "()" $ Right TokLParen
    ]

acceptTest :: TestTree
acceptTest =
  testGroup
    "acceptTest"
    [
        prettyTestCase accept []    $ Left "Nothing to accept"
      , prettyTestCase accept ""    $ Left "Nothing to accept"
      , prettyTestCase accept "*"   $ Right ""
      , prettyTestCase accept "(*)" $ Right "*)"
    ]
