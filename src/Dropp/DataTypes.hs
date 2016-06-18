{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


module Dropp.DataTypes where


import Data.Aeson
import Haxl.Core (GenHaxl)
import GHC.Generics (Generic)
import Data.Text.Internal (Text)
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
  , takeEnd)

import Data.Hashable
  ( Hashable
  , hash)

import Data.Aeson.Types (Parser)


-- ------------------------------------------------------------------------- --
--              ITEM AVAILABILITY
-- ------------------------------------------------------------------------- --

-- | Wrapper for a BangGood availability as obtained from the item page.
data Availability =
    Available -- ^ Item is available in an unspecified quantity.
  | AvCount Int -- ^ Item is available and the item count is given.
  | Low Int -- ^ Item available but the stock is nearly out.
  | Out -- ^ Item is not available.

  deriving Eq

-- | Define the sentences used in the email report overloading the Show class.
instance Show Availability where
    show Available = "Item available"
    show (AvCount n) = (show n) ++ " items available"
    show (Low n) = "Item low only " ++ (show n) ++ " pieces available"
    show Out = "Item not available"



-- | Exported smart constructor for the Availability data type. The function
-- gets the scraped availability string and parses it to generates the
-- appropriate type.
mkAv :: Text -> Maybe Availability

mkAv txt
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

instance FromJSON EbayStatus where
    parseJSON (String s) = case s of
        "on" -> pure On
        "off" -> pure Off
        _ -> empty

    parseJSON _ = empty

instance ToJSON EbayStatus


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
  , availability :: Maybe Text

    -- | Availabilty of the item on the ebay store.
  , onEbay :: Maybe EbayStatus}

  deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item


-- | Update the value of an item with the information that has to be fetched from
-- webpages.
updateItem
    :: Item -- ^ Initial item to be updated.
    -> Maybe Text -- ^ Availablity string as scraped from the shipper webpage.
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
instance ToJSON Env


-- | Provisional data structure as captured from the JSON object of variable items.
data JsonAv = JsonAv {itemCount :: Int}

  deriving (Show, Generic)

instance FromJSON JsonAv
instance ToJSON JsonAv
