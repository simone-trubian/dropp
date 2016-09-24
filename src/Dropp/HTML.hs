{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |This module contains HTML manipulation functions, in particular HTML
-- parsing and HTML generation. The module employs the
-- <https://hackage.haskell.org/package/lucid lucid> and
-- <https://hackage.haskell.org/package/html-conduit html-conduit> libraries.
module Dropp.HTML
  ( formatOutput
  , formatOutputItems
  , formatItem
  , renderAvailability
  , renderEbayStatus)
  where


import Dropp.DataTypes
import Lucid
import Safe (headMay)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Monoid ((<>))
import Data.Text
  ( Text
  , append
  , pack)

import Text.XML.Cursor
  ( Cursor
  , ($//)
  , (>=>)
  , attributeIs
  , fromDocument
  , element
  , content
  , child)


-- ------------------------------------------------------------------------- --
--              EMAIL
-- ------------------------------------------------------------------------- --

-- | Generate the entire HTML payload used as body of the report email.
formatOutput
    :: [ItemSnapshot] -- ^List of all pages scraped from the suppliers website.
    -> ByteString -- ^HTML payload of the report email
formatOutput items =
    renderBS
        $ ul_ [style_ "list-style-type:none; margin:10px 0"]
            $ mapM_ formatSnapItem items


formatOutputItems :: [Item] -> ByteString
formatOutputItems items =
    renderBS
        $ ul_ [style_ "list-style-type:none; margin:10px 0"]
            $ mapM_ formatItem items


formatSnapItem :: Monad m => ItemSnapshot -> HtmlT m ()
formatSnapItem itemSnap =
    li_
      $ ul_ [style_ "list-style-type:none; margin:10px 0"]
          ( renderItem (itemSnapId itemSnap) (sourceUrlSnap itemSnap) (itemNameSnap itemSnap)
         <> li_ (renderEbayStatus (ebayUrlSnap itemSnap) (prevStatusSnap itemSnap)
             <> toHtml (" -> " :: Text)
             <> renderEbayStatus (ebayUrlSnap itemSnap) (currentStatusSnap itemSnap))
         <> li_ (renderAvailability (prevAvSnap itemSnap)
             <> toHtml (" -> " :: Text)
             <> renderAvailability (currentAvSnap itemSnap)))

-- | Generate HTML list element comprised of:
--
-- [@Item name@] The name of the item, as defined in the item data base record
-- is formatted as an anchor. The href of the anchor is the sourceUrl of the
-- item record.
-- [@Ebay status@] Status of the Ebay store item as formatted by the
-- 'renderEbayStatus' function.
-- [@Source availability@] Availability of the item as formatted by the
-- 'renderAvaliablity' function.
formatItem :: Monad m => Item -> HtmlT m ()
formatItem item =
    li_
      $ ul_ [style_ "list-style-type:none; margin:10px 0"]
          ( renderItem (itemId item) (sourceUrl item) (itemName item)
         <> renderEbayStatus (ebayUrl item) (ebayStatus item)
         <> renderAvailability (availability item))


-- | Generate an HTML list item containing the item ID and item name.
renderItem :: Monad m => Int -> URL -> Text -> HtmlT m ()
renderItem iId iUrl iName = li_
                    ( toHtml (pack $ show iId)
                   <> toHtml (" - " :: Text)
                   <> a_ [ href_ iUrl
                         , style_ "color:black; text-decoration:none"]
                      (toHtml iName))


-- | Generate an HTML list item containing a colour-coded ebay status string.
-- The status string is color coded in the following manner:
--
-- [@On@] Green.
-- [@Off@] Orange.
-- [@Unrecognised@] Blue.
--
-- The ebay status is also an achor with its page url as href. The color
-- coding is achieved by modifying the style attribute of the <li> tag.
renderEbayStatus :: (Monad m, ToHTML a) => URL -> a -> HtmlT m ()
renderEbayStatus url status =
   a_
    [ href_ url
    , style_ $ "text-decoration:none; " `append` color status]
    (toHtml (message status))


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
renderAvailability :: (Monad m, ToHTML a) => a -> HtmlT m ()
renderAvailability ava = span_ [style_ (color ava)]
                              (toHtml $ message ava)



-- ------------------------------------------------------------------------- --
--              SCRAPING
-- ------------------------------------------------------------------------- --
-- | Implement the FromHTML interface by combining a cursor to the HTML
-- parsing function.
instance FromHTML Availability where
    decodeHTML = parseBangAva . makeCursor


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


-- | Generate a parsing cursor from an HTML page.
makeCursor :: ByteString -> Cursor
makeCursor = fromDocument . parseLBS



-- -- | Implement the FromHTML interface by combining a cursor to the HTML
-- -- parsing function.
-- instance FromHTML EbayStatus where
--     decodeHTML = parseEbayStatus . makeCursor

-- -- | Extract the status of an item on its Ebay page.
-- parseEbayStatus :: Cursor -> Maybe EbayStatus
-- parseEbayStatus cursor =
--   case spans of
--     Just node -> isOn node
--     Nothing -> Nothing
--
--   where
--     spans =
--         headMay $
--         cursor $//
--         element "span" >=>
--         attributeIs "class" "msgTextAlign"
--
--     isOn node =
--       case content <$> headMay (child node) of
--         Just [] -> Nothing
--         Just ["Questo oggetto è esaurito."] -> Just Off
--         Just [_] -> Nothing
--         Just (_:_) -> Nothing
--         Nothing -> Just On


-- -- | Return a minimal Ebay mock page containing the disclaimer string depending
-- -- on the value of the EbayStatus parameter.
-- ebayMockPage :: Monad m => EbayStatus -> HtmlT m ()
-- ebayMockPage status =
--   html_
--     $ body_
--       $ span_ [class_ "statusLeftContent"]
--         $ span_ [id_ "w1-3-_msg", class_ "msgTextAlign"]
--         (toHtml $ mockSentence status)
