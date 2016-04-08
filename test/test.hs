{-# LANGUAGE FlexibleContexts #-}


import HTML
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup)

import Test.Tasty.HUnit
  ( (@?=)
  , assertEqual
  , testCase)


main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [email]


email = testGroup "Email formatting tests"
  [ testCase "Available status is formatted green" undefined
  , testCase "More than five items are formatted green" undefined
  , testCase "Less than five items are formatted orange" undefined
  , testCase "No availability is formatted red" undefined
  , testCase "Unexpected availability is formatted blue"
    $ assertEqual "" (renderHtml $ formatBlock page) result]



page = pageTemplate "Status"
result = resultTemplate "Status" "color:blue"


pageTemplate :: String -> ByteString
pageTemplate status = pack $
    "<!DOCTYPE html><html><title>Title</title></head><body><div class=\"status\">"
    ++ status
    ++ "</div></body>"


resultTemplate :: String -> String -> ByteString
resultTemplate status color = pack $
    "<li><ul style=\"list-style-type:none; margin:10px 0\"><li>Title</li>"
    ++ "<li style=\"" ++ color ++ "\">"
    ++ status ++ "</li></ul></li>"
