import HTML
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup)

import Test.Tasty.HUnit
  ( (@?=)
  , testCase)


main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [email]


email = testGroup "Email formatting tests"
  [testCase "Inner list is formatted with no bulletpoints"
    $ (renderHtml $ formatBlock page) `compare` result @?= EQ]


page = "<!DOCTYPE html><html><title>Title</title></head><body><div class=\"status\">Status</div></body>"

result = "<li><ul style=\"list-style-type:none\"><li>Title</li><li>Status</li></ul></li>"
