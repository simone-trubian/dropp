module Main where


import BangDataSource
import Text.HTML.DOM (parseLBS)
import Haxl.Core
  ( GenHaxl
  , initEnv
  , stateSet
  , stateEmpty
  , runHaxl)

import Text.XML.Cursor
  ( ($//)
  , fromDocument
  , element
  , child)



type Haxl a = GenHaxl () a


getTitNode page = (fromDocument . parseLBS) page $// (element "title")


getTitCont = head . child . head . getTitNode


getAvailability :: Haxl a -> IO a
getAvailability fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches


urls =
    [ "http://eu.banggood.com/Wholesale-Warehouse-1080P-HDMI-Male-To-VGA-Female-Adapter-Video-Converter-Cable-wp-Uk-937626.html"
    , "http://eu.banggood.com/Wholesale-Warehouse-Silver-Stainless-Steel-Pocket-Cigar-Cutter-Knife-Double-Blades-wp-Uk-44851.html"]

main :: IO ()
main = do
    pages <- getAvailability $ mapM getHTML urls
    print $ map getTitCont pages
