import Test.Tasty
import Test.Tasty.HUnit


main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [email]


email = testGroup "Email formatting tests"
  [testCase "Doesn't do anything" $ True `compare` True @?= EQ]
