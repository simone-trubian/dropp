module HTML
  ( formatOutput)
  where


import Data.Text.Internal (Text)
import Data.Text (pack)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
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
        show title ++ "\n" ++ show availability ++ "\n"


-- ------------------------------------------------------------------------- --
--              BANGGOOD
-- ------------------------------------------------------------------------- --

formatOutput :: [ByteString] -> Text
formatOutput = pack . concatMap ((++"\n") . show . makeBlock)


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
