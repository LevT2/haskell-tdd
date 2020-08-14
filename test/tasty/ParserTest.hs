module ParserTest where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser08.Parser (Token(..), lookAhead)

prettyTestCaseShow :: Show a => TestName -> a -> TestTree
prettyTestCaseShow prettyOut construct =
  testCase prettyOut $
    show construct @?= prettyOut

prettyTestCase :: (Eq a, Show a) => (String -> a) -> String -> a -> TestTree
prettyTestCase f input result =
  let prettyIn = if null input then "Empty input" else input in
  testCase prettyIn $
    f input @?= result

testParser :: TestTree
testParser = testGroup "all" testTokenizer

testTokenizer = [
  testCase "" $ (lookAhead []) @?= TokEnd,
  prettyTestCase lookAhead [] TokEnd,
  prettyTestCase lookAhead "()" TokLParen
  ]