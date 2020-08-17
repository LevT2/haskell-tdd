{-# LANGUAGE ScopedTypeVariables #-}

module ExampleTest where

import Test.Tasty
import Test.Tasty.HUnit

test_generateTree :: IO TestTree
test_generateTree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()
