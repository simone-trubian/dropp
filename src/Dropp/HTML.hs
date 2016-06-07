{-# LANGUAGE FlexibleContexts #-}

-- |This module contains HTML manipulation functions, in particular HTML
-- parsing and HTML generation. The module employs the
-- <https://hackage.haskell.org/package/lucid lucid> and
-- <https://hackage.haskell.org/package/html-conduit html-conduit> libraries.
module Dropp.HTML
  ( formatOutput
  , formatItem
  , formatItemCount
  , renderAvailability
  , scrapeEbayStatus
  , scrapeBGAv
  , bangGoodMockPage
  , ebayMockPage)
  where


import Dropp.DataTypes
import Lucid
import Data.Maybe (fromJust)
import Safe (headMay)
import Data.Text.Internal (Text)
import Data.Text (pack)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
import Text.Parsec.Pos (SourceName)
import Text.Parsec.Error (ParseError)

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


-- | Generate HTML list comprised of an item name, coming from the item page
-- title and its colour-coded availability string.
formatItem :: Monad m => Item -> HtmlT m ()
formatItem item =
    li_
      $ ul_ [style_ "list-style-type:none; margin:10px 0"]
         $ do li_ (toHtml $ item_name item)
              renderAvailability item


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
renderAvailability :: Monad m => Item-> HtmlT m ()
renderAvailability item = li_ [style_ (color content)] (toHtml content)
  where
    content :: Text
    content = case availability item of
        (Just av) -> av
        Nothing -> "could not fetch item."

    color txt
      | txt == "Currently out of stock" = "color:red"
      | txt == "In stock, usually dispatched in 1 business day" = "color:green"
      | otherwise = formatItemCount txt


-- | Generate a styling attribute value starting from a parsed availability
-- string. The function will try to parse any number present in the string and
-- use that as  the number of items currently in stock. If the number of items
-- is > 5 the item is considered available and marked greeen. If the number of
-- items is <= 5 the items is considered "going quick" and marked orange. If
-- the parser fails the availability string is different from all expected
-- values and therefore marked blue.
formatItemCount :: Text -> Text
formatItemCount txt =
  case parse parser "" txt of
    Right str -> decideCount str
    Left _ -> "color:blue"

  where
    -- | Parse the first occurrence of a number of any number of digits is a
    -- string of text. The parser will stop at the first number found and fail
    -- if no numbers are found.
    parser = many (noneOf ['0'..'9']) >> many1 digit

    decideCount str =
      -- Check if the number of items is > 5.
      if read str > 5 then "color:green" else "color:orange"


-- ------------------------------------------------------------------------- --
--              MOCK PAGES
-- ------------------------------------------------------------------------- --

bangGoodMockPage :: Monad m => Item -> HtmlT m ()
bangGoodMockPage block =
    html_ $ do
      title_ (toHtml $ item_name block)
      body_ (div_ [class_ "status"] (toHtml $ fromJust $ availability block))


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
scrapeBGAv :: ByteString -> Maybe Text
scrapeBGAv = parseBangAva . makeCursor


-- | Extract the availability of a BangGood item page from the cursor opened on
-- that page.
parseBangAva :: Cursor -> Maybe Text
parseBangAva cursor =
  case divs of
    Just xs -> headMay $ content xs
    Nothing -> Nothing

  where
    divs = headMay $
        cursor $//
        element "div" >=>
        attributeIs "class" "status" >=>
        child


scrapeEbayStatus :: ByteString -> Maybe EbayStatus
scrapeEbayStatus = parseEbayStatus . makeCursor


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
      case content <$> (headMay $ child node) of
        Just isOffSentence-> Just Off
        Nothing -> Just On


-- | Generate an item block starting from a BangGood item page.
makeBlock :: ByteString -> Item
makeBlock = undefined


-- | Generate a parsing cursor from an HTML page.
makeCursor :: ByteString -> Cursor
makeCursor = fromDocument . parseLBS


isOffSentence = "Questa inserzione è stata chiusa dal venditore perché l'oggetto non è più disponibile." :: String
