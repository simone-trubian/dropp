module HTML
  ( formatOutput)
  where


import Data.Text.Internal (Text)
import Data.Text (pack)
import Text.HTML.DOM (parseLBS)
import Data.ByteString.Lazy.Internal (ByteString)
import Text.XML.Cursor
  ( ($//)
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


makeBlock page = Email getTitle getAvailability
    where
        getTitle = parseTitle $ (fromDocument . parseLBS) page
        getAvailability = parseAvailability $ (fromDocument . parseLBS) page


getTitleNode = parseTitle . fromDocument . parseLBS


getAvailabilityNode = parseAvailability . fromDocument . parseLBS


formatOutput :: [ByteString] -> Text
formatOutput = pack . concatMap ((++"\n") . show . makeBlock)
