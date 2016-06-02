{-# LANGUAGE FlexibleContexts #-}

-- |This module contains HTML manipulation functions, in particular HTML
-- parsing and HTML generation. The module employs the
-- <https://hackage.haskell.org/package/lucid lucid> and
-- <https://hackage.haskell.org/package/html-conduit html-conduit> libraries.
module Dropp.HTML
  ( formatOutput
  , formatBlock
  , formatItemCount
  , renderAvailability
  , scrapeEbayStatus 
  , bangGoodMockPage
  , ebayMockPage)
  where


import Dropp.DataTypes
import Lucid
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
--              BANGGOOD
-- ------------------------------------------------------------------------- --

-- | Generate the entire HTML payload used as body of the report email.
formatOutput
    :: [ByteString] -- ^List of all pages scraped from the suppliers website.
    -> ByteString -- ^HTML payload of the report email
formatOutput pages = renderBS $ ul_ $ mapM_ formatBlock pages


-- | Generate HTML list comprised of an item name, coming from the item page
-- title and its colour-coded availability string.
formatBlock :: Monad m => ByteString -> HtmlT m ()
formatBlock page =
    li_
      $ ul_ [style_ "list-style-type:none; margin:10px 0"]
         $ do li_ (toHtml $ item_name block)
              renderAvailability block

  where
    block = makeBlock page


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
renderAvailability block = li_ [style_ (color content)] (toHtml content)
  where
    -- content :: Item -> Text
    content = case availability block of
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


-- | Generate an item block starting from a BangGood item page.
makeBlock :: ByteString -> Item
makeBlock = undefined
-- makeBlock page = ItemBlock getTitle getAvailability
--   where
--     getTitle = parseBangTitle cursor
--     getAvailability = parseBangAva cursor
--     cursor = makeCursor page


-- | Generate a parsing cursor from an HTML page.
makeCursor :: ByteString -> Cursor
makeCursor = fromDocument . parseLBS


-- | Extract the title of a BangGood item page from the cursor opened on that
-- page.
parseBangTitle :: Cursor -> Text
parseBangTitle cursor =
    head . content . head $
    cursor $//
    element "title" >=>
    child


-- | Extract the availability of a BangGood item page from the cursor opened on
-- that page.
parseBangAva :: Cursor -> Text
parseBangAva cursor =
    head . content . head $
    cursor $//
    element "div" >=>
    attributeIs "class" "status" >=>
    child


bangGoodMockPage :: Monad m => Item -> HtmlT m ()
bangGoodMockPage = undefined
--bangGoodMockPage block =
--    html_ $ do
--      title_ (toHtml $ item_name block)
--      body_ (div_ [class_ "status"] (toHtml $ availability block))


-- ------------------------------------------------------------------------- --
--              EBAY
-- ------------------------------------------------------------------------- --

ebayMockPage :: Monad m => EbayStatus -> HtmlT m ()
ebayMockPage isOn = html_ $ do

    let sentence = "Questa inserzione è stata chiusa dal venditore perché l'oggetto non è più disponibile." :: String

    let banner = span_ [class_ "statusLeftContent"]
          $ span_ [id_ "w1-3-_msg", class_ "msgTextAlign"]
            (toHtml sentence)

    let decoy = span_ $ p_ "bblbl"

    let content = case isOn of
          On -> banner
          Off -> decoy

    body_ content


scrapeEbayStatus :: ByteString -> Maybe EbayStatus
scrapeEbayStatus = undefined
