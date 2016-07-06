{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


module Dropp.DataTypes where


import Data.Aeson
import GHC.Generics (Generic)
import Data.Text.Internal (Text)
import qualified Data.HashMap.Lazy as HML (lookup)
import Data.ByteString.Lazy.Internal (ByteString)
import Control.Applicative (empty)
import Control.Monad (mzero)
import Text.Parsec
  ( many1
  , many
  , noneOf
  , digit
  , parse)

import Data.Text
  ( pack
  , append)


-- ------------------------------------------------------------------------- --
--              HTML INTERFACE DEFINITION
-- ------------------------------------------------------------------------- --

-- | Get a Dropp data structure from a scraped page.
class FromHTML a where
    -- | Defines a common method for all scraped data types that goes from the
    -- scraped webpage body to a Maybe data type. This method in meant to
    -- be similar to the decode function used for JSON and YAML deserialisation.
    decodeHTML :: ByteString -> Maybe a


-- | Go from a Dropp data structure to a Text that can be used to generate HTML.
class ToHTML a where
    -- | Generate the string shown in the report email for the specific type.
    message :: a -> Text
    -- | Generate the color attribute used in the report email.
    color :: a -> Text
    -- | Generate the sentence that has to be scraped from the original HTML
    -- page to generate the data type. This method is used in generating mock
    -- pages.
    mockSentence :: a -> Text


-- ------------------------------------------------------------------------- --
--              BASE CLASSES OVERLOADING
-- ------------------------------------------------------------------------- --

-- | All Dropp scraped data types are returned boxed in a Maybe value.
instance (ToHTML a) => ToHTML (Maybe a) where
    message (Just a) = message a
    message Nothing = "could not fetch update"

    color (Just a) = color a
    color Nothing = "color:blue"

    -- | The implementation of this method is in practice useless.
    mockSentence (Just a) = mockSentence a
    mockSentence Nothing = ""


-- | Dummy implementation of a list of data types to be converted from HTML.
-- This implementation is semantically nonsense, because there is no such thing
-- as a list of HTML pages. However it is necessary to type-check the
-- `fetchHttp` function.

instance (FromHTML a) => FromHTML [a] where
    decodeHTML _ = Nothing


-- ------------------------------------------------------------------------- --
--              ITEM AVAILABILITY
-- ------------------------------------------------------------------------- --

-- | Wrapper for a BangGood availability as obtained from the item page.
data Availability =
    Available -- ^ Item is available in an unspecified quantity.
  | AvCount Int -- ^ Item is available and the item count is given.
  | Low Int -- ^ Item available but the stock is nearly out.
  | Out -- ^ Item is not available.

  deriving (Show, Eq, Generic)


-- | Define the sentences used in the email report overloading the ToHTML class.
instance ToHTML Availability where
    message Available = "Item available"
    message (AvCount n) = pack (show n) `append` " items available"
    message (Low n) = "Item low only "
                      `append` pack (show n)
                      `append` " pieces available"
    message Out = "Item not available"

    color Available = "color:green"
    color (AvCount _) = "color:green"
    color (Low _) = "color:orange"
    color Out = "color:red"

    mockSentence Available = "In stock, usually dispatched in 1 business day"
    mockSentence Out = "Currently out of stock"
    mockSentence (AvCount n) =
        pack $ show n ++ " units, dispatched in 1 business day"
    mockSentence (Low n) =
        pack $ "Only " ++ show n ++ " units, dispatched in 1 business day"


-- | Custom implementation of de-serialisation for the Availability data type.
-- The need for a custom implementation arises from the format of the JSON
-- object returned by the server, the structure of is:
--
--{
--  "hideBuy": Number,
--  "message": String,
--  "stocks": Number,
--  "show_stocks": Number,
--  "hideSku": Number,
--  "final_price": Number,
--  "price": Number,
--  "format_price": String,
--  "currency_tags": String,
--  "discount": String
--}
--
-- Of all the fields the only useful is `message` that is exactly the same
-- message scraped by the website.
instance FromJSON Availability where
    parseJSON (Object o) = case HML.lookup (pack "message") o of
        Just (String s) -> parseString s
        _ -> empty

        where
          parseString s = case mkAvailability s of
              (Just Out) -> pure Out
              (Just Available) -> pure Available
              (Just (AvCount n)) -> pure (AvCount n)
              (Just (Low n)) -> pure (Low n)
              Nothing -> empty


-- | Partial implementation of serialisation of the Availability data type.
-- Only the `message` part of the full object is implemented so that it can be
-- used by the local dev server.
instance ToJSON Availability where
    toJSON Out = object ["message" .= String (mockSentence Out)]
    toJSON Available = object ["message" .= String (mockSentence Available)]
    toJSON (AvCount n) = object ["message" .= String (mockSentence (AvCount n))]
    toJSON (Low n) = object ["message" .= String (mockSentence (Low n))]

-- | Exported smart constructor for the Availability data type. The function
-- gets the scraped availability string and parses it to generates the
-- appropriate type.
mkAvailability :: Text -> Maybe Availability

mkAvailability txt
  | txt == "Currently out of stock" = Just Out
  | txt == "In stock, usually dispatched in 1 business day" = Just Available
  | otherwise = getAvCount

  where
    -- | Parse the number in the string and return the right type.
    getAvCount =
      case parse parser "" txt of
        Right str -> decideCount str
        Left _ -> Nothing

    -- | Parse the first occurrence of a number of an arbitrary number of digits
    -- in a string of text. The parser will return the first number found and
    -- fail if no numbers are found.
    parser = many (noneOf ['0'..'9']) >> many1 digit

    -- | Check if the number of items is > 5.
    decideCount str =
        if read str > threshold then Just (AvCount (read str)) else Just (Low (read str))
      where
        threshold :: Int
        threshold = 5


-- ------------------------------------------------------------------------- --
--              EBAY STATUS
-- ------------------------------------------------------------------------- --

-- | Type alias for an ebay status.
data EbayStatus =
    On -- ^ Item is available to purchase from Ebay.
  | Off -- ^ Item has been removed from the Ebay store.

  deriving (Show, Ord, Eq, Generic)


-- | Define the sentences used in the email report overloading the ToHTML class.
instance ToHTML EbayStatus where
    message On = "on ebay"
    message Off = "off ebay"

    color On = "color:green"
    color Off = "color:red"

    mockSentence On = ""
    mockSentence Off = "Questa inserzione è stata chiusa dal venditore perché l'oggetto non è più disponibile."


-- | Overloaded instance (Still unused) for decoding JSON to the data type.
instance FromJSON EbayStatus where
    parseJSON (String s) = case s of
        "on" -> pure On
        "off" -> pure Off
        _ -> empty

    parseJSON _ = empty


-- ------------------------------------------------------------------------- --
--              ITEM
-- ------------------------------------------------------------------------- --

data Item = Item
  { -- | URL of item page from the shipper website.
    sourceUrl :: URL

    -- | URL of the ebay store page of the same item.
  , ebayUrl :: URL

    -- | Name of the item.
  , itemName :: Text

    -- | Availability of the item as fetched from the shipper website.
  , availability :: Maybe Availability

    -- | Availabilty of the item on the ebay store.
  , ebayStatus :: Maybe EbayStatus}

  deriving (Show, Generic)


instance FromJSON Item where
    parseJSON (Object o) =
        Item <$> o .: "source_url"
             <*> o .: "ebay_url"
             <*> o .: "item_name"
             <*> o .:? "availability"
             <*> o .:? "ebayStatus"

    parseJSON _ = mzero


-- | Dummy implementation of the FromHTML type class. This boilerplate is
-- necessary to allow type-checking of the `fetchHttp` function, even though
-- trying to parse an Item from a HTML page is nonsense and therefore considered
-- an error.
instance FromHTML Item where
    decodeHTML _ = Nothing


-- | Update the value of an item with the information that has to be fetched
-- from webpages.
updateItem
    :: Item -- ^ Initial item to be updated.
    -> Maybe Availability -- ^ Shipper availability data type.
    -> Maybe EbayStatus -- ^ Availabity status as scraped from the ebay store.
    -> Item -- ^ Updated Item.
updateItem item av status = newItem
  where
    newItem = partialItem {availability = av}
    partialItem = item {ebayStatus = status}


-- ------------------------------------------------------------------------- --
--              ENVIRONMENT
-- ------------------------------------------------------------------------- --

-- | Environment variables data type.
data DroppEnv = Env
  { -- | List of email addresses of the report email recipients.
    recipients :: [Text]

    -- | Email address of the report email sender.
  , sender :: Text

    -- | URL of the database endpoint returning the inital list of items.
  , dbItemsUrl :: Text

    -- | Flag that allows sending the report email. If false the email body is
    -- dumped to a file, set to False for testing and True for production.
  , sendEmail :: Bool

    -- | Relative file path of the email dump file. This is not to be used in
    -- production but for testing only.
  , emailDumpFilePath :: String}

  deriving (Show, Generic)

instance FromJSON DroppEnv


-- ------------------------------------------------------------------------- --
--              OTHER TYPES
-- ------------------------------------------------------------------------- --

-- | Data representing the content type of an HTTP response.
data MimeType =
    TextHtml -- ^ HTML page.
  | ApplicationJson -- ^ JSON object.


type URL = Text
