{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


module Dropp.DataTypes where


import Data.Aeson
import Haxl.Core (GenHaxl)
import GHC.Generics (Generic)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Internal (ByteString)
import Text.Parsec
  ( many1
  , many
  , noneOf
  , digit
  , parse)

import Control.Applicative
  ( pure
  , empty)

import Data.Text
  ( pack
  , unpack
  , append
  , takeEnd)

import Data.Hashable
  ( Hashable
  , hash)

import Data.Aeson.Types (Parser)


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


-- ------------------------------------------------------------------------- --
--              MAYBE OVERLOADING
-- ------------------------------------------------------------------------- --

-- | All Dropp scraped data types are returned boxed in Maybe.
instance (ToHTML a) => ToHTML (Maybe a) where
    message (Just a) = message a
    message Nothing = "could not fetch update"

    color (Just a) = color a
    color Nothing = "color:blue"


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

instance FromJSON Availability



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
    -- in a string of text. The parser will return the first number found and fail
    -- if no numbers are found.
    parser = many (noneOf ['0'..'9']) >> many1 digit

    -- | Check if the number of items is > 5.
    decideCount str =
      if read str > 5 then Just (AvCount (read str)) else Just (Low (read str))


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
    source_url :: Text

    -- | URL of the ebay store page of the same item.
  , ebay_url :: Text

    -- | Name of the item.
  , item_name :: Text

    -- | Availability of the item as fetched from the shipper website.
  , availability :: Maybe Availability

    -- | Availabilty of the item on the ebay store.
  , onEbay :: Maybe EbayStatus}

  deriving (Show, Generic)

instance FromJSON Item


-- | Update the value of an item with the information that has to be fetched from
-- webpages.
updateItem
    :: Item -- ^ Initial item to be updated.
    -> Maybe Availability -- ^ Shipper availability data type.
    -> Maybe EbayStatus -- ^ Availabity status as scraped from the ebay store.
    -> Item -- ^ Updated Item.
updateItem item availability ebayStatus = newItem
  where
    newItem = partialItem {availability = availability}
    partialItem = item {onEbay = ebayStatus}

-- ------------------------------------------------------------------------- --
--              ENVIRONMENT
-- ------------------------------------------------------------------------- --

-- | Environment variables data type.
data Env = Env
  { -- | List of email addresses of the report email recipients.
    recipients :: [Text]

    -- | Email address of the report email sender.
  , sender :: Text

    -- | URL of the database endpoint returning the inital list of items.
  , dbItemsUrl :: Text }

  deriving (Show, Generic)

instance FromJSON Env


-- | Provisional data structure as captured from the JSON object of variable items.
data JsonAv = JsonAv {itemCount :: Int}

  deriving (Show, Generic)

instance FromJSON JsonAv
instance ToJSON JsonAv
