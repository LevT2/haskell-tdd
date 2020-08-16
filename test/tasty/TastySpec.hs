module TastySpec where

import Test.Tasty (TestTree, defaultMain, testGroup)

import ParserTest (testParser)

main :: IO ()
main = defaultMain $
    testGroup "all" [testParser]
