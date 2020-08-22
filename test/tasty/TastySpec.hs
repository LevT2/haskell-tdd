import Test.Tasty (TestTree, defaultMain, testGroup)

import ParserTest 

main :: IO ()
main = defaultMain $
    testGroup "all" [test]
