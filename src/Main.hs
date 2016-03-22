module Main where


import BangDataSource
import Data.Time.Clock (getCurrentTime)
import Text.HTML.DOM (parseLBS)
import Data.Text.Internal (Text)
import Haxl.Core
  ( GenHaxl
  , initEnv
  , stateSet
  , stateEmpty
  , runHaxl)

import Text.XML.Cursor
  ( ($//)
  , ($|)
  , (>=>)
  , attributeIs
  , fromDocument
  , element
  , content
  , child)



type Haxl a = GenHaxl () a


data Email = Email
  { title :: Text
  , availability :: Text}

instance Show Email where
    show (Email title availability) = show title ++ "\n" ++ show availability


urls =
    [ "http://eu.banggood.com/Wholesale-Warehouse-1080P-HDMI-Male-To-VGA-Female-Adapter-Video-Converter-Cable-wp-Uk-937626.html"
    , "http://eu.banggood.com/Wholesale-Warehouse-Silver-Stainless-Steel-Pocket-Cigar-Cutter-Knife-Double-Blades-wp-Uk-44851.html"]


main :: IO ()
main = do
    pages <- getPages $ mapM getHTML urls
    now <- getCurrentTime
    let formatOutput = (concatMap (++"\n")) . (map show) . map makeBlock
    print $ (show now) ++ "\n" ++ formatOutput pages


parseTitle cursor =
    head . content . head $
    cursor $//
    element "title" >=>
    child


parseAvailability cursor =
    head . content . head $
    cursor $//
    element "div" >=>
    attributeIs "class" "status" >=>
    child


makeBlock page = Email (getTitle) (getAvailability)

    where
        getTitle = parseTitle $ (fromDocument . parseLBS) page
        getAvailability = parseAvailability $ (fromDocument . parseLBS) page

getTitleNode = parseTitle . (fromDocument . parseLBS)


getAvailabilityNode = parseAvailability . (fromDocument . parseLBS)


getPages :: Haxl a -> IO a
getPages fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches
