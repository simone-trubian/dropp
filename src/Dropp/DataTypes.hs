{-# LANGUAGE DeriveGeneric #-}


module Dropp.DataTypes where


import Data.Aeson
import Haxl.Core (GenHaxl)
import GHC.Generics (Generic)
import Data.Text.Internal (Text)
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
