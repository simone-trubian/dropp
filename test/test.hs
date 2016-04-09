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


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [email]


email :: TestTree
email = testGroup "Email formatting tests"
  [ testCase "Available status is formatted green"
      $ assertFormatting
          "In stock, usually dispatched in 1 business day"
          "color:green"

  , testCase "More than five items are formatted green"
      $ assertFormatting
          "Only 10 units,dispatched in 1 business day"
          "color:green"

  , testCase "Less than five items are formatted orange"
      $ assertFormatting
          "Only 5 units,dispatched in 1 business day"
          "color:orange"

  , testCase "No availability is formatted red"
      $ assertFormatting "Currently out of stock" "color:red"

  , testCase "Unexpected availability is formatted blue"
      $ assertFormatting "Status" "color:blue"]


-- | Assert that availability string is formatted with the right colour.
assertFormatting status color =
    assertEqual
        ""
        (renderHtml $ formatBlock (pageTemplate status))
        (resultTemplate status color)


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
