{-# LANGUAGE ScopedTypeVariables #-}

module ExampleTest where

import Parser08.Parser (Token (..), lookAhead, accept)

import Test.Tasty
import Test.Tasty.HUnit

test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

unit_empty1 = lookAhead "" @?= Right TokLParen

