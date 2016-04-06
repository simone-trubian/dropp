module HTML
  ( Email (Email)
  , formatOutput
  , formatBlock)
  where


import Data.Text.Internal (Text)
import Data.Text (pack)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5.Attributes (style)
import Text.Blaze.Html5
  ( Html
  , toHtml
  , ul
  , li
  , (!))


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


-- ------------------------------------------------------------------------- --
--              TYPES
-- ------------------------------------------------------------------------- --

data Email = Email
  { title :: Text
  , availability :: Text}

instance Show Email where
    show (Email title availability) =
        show title ++ "\n" ++ show availability


-- ------------------------------------------------------------------------- --
--              BANGGOOD
-- ------------------------------------------------------------------------- --

formatOutput :: [ByteString] -> ByteString
formatOutput pages = renderHtml $ ul $ mapM_ formatBlock pages


formatBlock :: ByteString -> Html
formatBlock page = li $ (ul $ do
    li $ toHtml $ title $ makeBlock page
    li $ toHtml $ availability $ makeBlock page)
    ! style "list-style-type:none; margin:10px 0"


makeBlock :: ByteString -> Email
makeBlock page = Email getTitle getAvailability
  where
    getTitle = parseTitle $ makeCursor page
    getAvailability = parseAvailability $ makeCursor page


makeCursor :: ByteString -> Cursor
makeCursor = fromDocument . parseLBS


parseTitle :: Cursor -> Text
parseTitle cursor =
    head . content . head $
    cursor $//
    element "title" >=>
    child


parseAvailability :: Cursor -> Text
parseAvailability cursor =
    head . content . head $
    cursor $//
    element "div" >=>
    attributeIs "class" "status" >=>
    child
