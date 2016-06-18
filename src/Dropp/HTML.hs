{-# LANGUAGE FlexibleContexts #-}

-- |This module contains HTML manipulation functions, in particular HTML
-- parsing and HTML generation. The module employs the
-- <https://hackage.haskell.org/package/lucid lucid> and
-- <https://hackage.haskell.org/package/html-conduit html-conduit> libraries.
module Dropp.HTML
  ( formatOutput
  , formatItem
  , renderAvailability
  , renderEbayStatus
  , scrapeEbayStatus
  , scrapeBGAv
  , bangGoodMockPage
  , ebayMockPage)
  where


import Dropp.DataTypes
import Lucid
import Safe (headMay)
import Data.Text.Internal (Text)
import Data.Text (pack)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
import Text.Parsec.Pos (SourceName)
import Text.Parsec.Error (ParseError)
import Data.Maybe
  ( fromJust
  , fromMaybe)

import Text.XML.Cursor
  ( Cursor
  , ($//)
  , ($|)
  , (>=>)
  , attributeIs
  , fromDocument
  , element
  , content
  , child)

import Text.Parsec
  ( many1
  , many
  , noneOf
  , digit
  , parse)


-- ------------------------------------------------------------------------- --
--              EMAIL
-- ------------------------------------------------------------------------- --

-- | Generate the entire HTML payload used as body of the report email.
formatOutput
    :: [Item] -- ^List of all pages scraped from the suppliers website.
    -> ByteString -- ^HTML payload of the report email
formatOutput items = renderBS $ ul_ $ mapM_ formatItem items


-- | Generate HTML list element comprised of:
--
-- [@Item name@] The name of the item, as defined in the item data base record
-- is formatted as an anchor. The href of the anchor is the source_url of the item
-- record.
-- [@Ebay status@] Status of the Ebay store item as formatted by the
-- 'renderEbayStatus' function.
-- [@Source availability@] Availability of the item as formatted by the
-- 'renderAvaliablity' function.
formatItem :: Monad m => Item -> HtmlT m ()
formatItem item =
    li_
      $ ul_ [style_ "list-style-type:none; margin:10px 0"]
         $ do li_ (a_ [href_ (source_url item), style_
                       "color:black; text-decoration:none"]
                   (toHtml $ item_name item))
              renderEbayStatus item
              renderAvailability $ availability item


-- | Generate an HTML list item containing a colour-coded ebay status string.
-- The status string is color coded in the following manner:
--
-- [@On@] Green.
-- [@Off@] Orange.
-- [@Unrecognised@] Blue.
--
-- The ebay status is also an achor with its page url as href. The color
-- coding is achieved by modifying the style attribute of the <li> tag.
renderEbayStatus :: Monad m => Item -> HtmlT m ()
renderEbayStatus item =
    li_
        (a_ [href_ (ebay_url item), style_ color] (toHtml content))
  where
    content :: Text
    content = case onEbay item of
        Just On -> "on ebay"
        Just Off -> "off ebay"
        Nothing -> "could not fetch ebay status"

    color = case onEbay item of
        Just On -> "color:green"
        Just Off -> "color:red"
        Nothing -> "color:blue"


-- | Generate an HTML list item containing a colour-coded availabilty string.
-- The availability string is color coded in the following manner:
--
-- [@Available@] Green.
-- [@Low level@] Orange.
-- [@Out of stock@] Red.
-- [@Unrecognised@] Blue.
--
-- The color coding is achieved by modifying the style attribute of the <li>
-- tag.
renderAvailability :: Monad m => Maybe Availability -> HtmlT m ()
renderAvailability av = li_ [style_ (color av)] (toHtml $ content av)
  where

    content :: Maybe Availability -> Text
    content (Just av) = pack $ show av
    content Nothing = "could not fetch item availability"

    color :: Maybe Availability -> Text
    color (Just Available) = "color:green"
    color (Just (AvCount _)) = "color:green"
    color (Just (Low _)) = "color:orange"
    color (Just Out) = "color:red"
    color Nothing = "color:blue"


-- ------------------------------------------------------------------------- --
--              MOCK PAGES
-- ------------------------------------------------------------------------- --

-- | Return a minimal page containing only the title and availability.
bangGoodMockPage :: Monad m => Item -> HtmlT m ()
bangGoodMockPage item =
    html_ $ do
      title_ (toHtml $ item_name item)
      body_ (div_ [class_ "status"] (toHtml ("In stock, usually dispatched in 1 business day" :: String)))


-- | Return a minimal Ebay mock page containing the disclaimer string depending
-- on the value of the EbayStatus parameter.
ebayMockPage :: Monad m => EbayStatus -> HtmlT m ()
ebayMockPage isOn = html_ $ do

    let banner = span_ [class_ "statusLeftContent"]
          $ span_ [id_ "w1-3-_msg", class_ "msgTextAlign"]
            $ toHtml isOffSentence

    let emptyBanner = span_ [class_ "statusLeftContent"]
          $ span_ [id_ "w1-3-_msg", class_ "msgTextAlign"]
            $ toHtml ("" :: String)

    let content = case isOn of
          On -> banner
          Off -> emptyBanner

    body_ content



-- ------------------------------------------------------------------------- --
--              SCRAPING
-- ------------------------------------------------------------------------- --
-- | Combine cursor to the HTML parsing function.
scrapeBGAv :: ByteString -> Maybe Availability
scrapeBGAv = parseBangAva . makeCursor


-- | Extract the availability of a BangGood item page from the cursor opened on
-- that page.
parseBangAva :: Cursor -> Maybe Availability
parseBangAva cursor =
  case pif of
    Just xs -> mkAvailability xs
    Nothing -> Nothing

  where
    pif = case divs of
      Just xs -> headMay $ content xs
      Nothing -> Nothing

    divs =
        headMay $
        cursor $//
        element "div" >=>
        attributeIs "class" "status" >=>
        child


-- | Combine cursor to the HTML parsing function.
scrapeEbayStatus :: ByteString -> Maybe EbayStatus
scrapeEbayStatus = parseEbayStatus . makeCursor


-- | Extract the status of an item on its Ebay page.
parseEbayStatus :: Cursor -> Maybe EbayStatus
parseEbayStatus cursor =
  case spans of
    Just node -> isOn node
    Nothing -> Nothing

  where
    spans =
        headMay $
        cursor $//
        element "span" >=>
        attributeIs "class" "msgTextAlign"

    isOn node =
      case content <$> headMay (child node) of
        Just isOffSentence-> Just Off
        Nothing -> Just On


-- | Generate a parsing cursor from an HTML page.
makeCursor :: ByteString -> Cursor
makeCursor = fromDocument . parseLBS


isOffSentence = "Questa inserzione è stata chiusa dal venditore perché l'oggetto non è più disponibile." :: String
