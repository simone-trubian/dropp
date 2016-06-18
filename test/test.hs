{-# LANGUAGE FlexibleContexts #-}


import Dropp.HTML
import Dropp.DataTypes
import qualified Lucid as L
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (maybe)

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
  [ testCase "Available status is formatted green - on ebay is formatted green"
      $ assertFormatting
          (Just Available)
          "color:green"
          (Just On)
          "on ebay"
          "color:green"

  , testCase "More than five items are formatted green - on ebay is formatted green"
      $ assertFormatting
          (Just (AvCount 10))
          "color:green"
          (Just On)
          "on ebay"
          "color:green"

  , testCase "Less than five items are formatted orange - off ebay is formatted red"
      $ assertFormatting
          (Just (Low 5))
          "color:orange"
          (Just Off)
          "off ebay"
          "color:red"

  , testCase "No availability is formatted red - off ebay is formatted red"
      $ assertFormatting
          (Just Out)
          "color:red"
          (Just Off)
          "off ebay"
          "color:red"

  , testCase "Unexpected availability is formatted blue - no ebay status is formatted blue"
      $ assertFormatting
          Nothing
          "color:blue"
          Nothing
          "could not fetch ebay status"
          "color:blue"
  ]


-- | Assert that availability string is formatted with the right colour.
assertFormatting status color ebayStatus ebayString ebayColor =
    assertEqual
        ""
        (L.renderBS $ formatItem
          (Item
            "http://source.com"
            "http://ebay.com"
            "Title"
            status
            ebayStatus))
        (resultTemplate (maybe "could not fetch item availability" show status) color ebayString ebayColor)


-- resultTemplate :: String -> String -> ByteString
resultTemplate status color ebayStatus ebayColor = pack $
    "<li><ul style=\"list-style-type:none; margin:10px 0\">"
    ++ "<li><a style=\"color:black; text-decoration:none\" href=\"http://source.com\">Title</a></li>"

    ++ "<li>"
    ++ "<a style=\"" ++ ebayColor ++ "\" href=\"http://ebay.com\">"
        ++ ebayStatus ++ "</a></li>"

    ++ "<li style=\"" ++ color ++ "\">"
    ++ status ++ "</li>"
    ++ "</ul></li>"
