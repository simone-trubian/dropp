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
data EbayStatus = On | Off

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
  { source_url :: Text
  , ebay_url :: Text
  , item_name :: Text
  , availability :: Maybe Text
  , onEbay :: Maybe EbayStatus}

  deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item


updateItem :: Item -> Maybe Text -> Maybe EbayStatus -> Item
updateItem item availability ebayStatus = newItem
  where
    newItem = partialItem {availability = availability}
    partialItem = item {onEbay = ebayStatus}

-- ------------------------------------------------------------------------- --
--              ENVIRONMENT
-- ------------------------------------------------------------------------- --

-- | Environment variables data type.
data Env = Env
  { recipients :: [Text]
  , sender :: Text
  , dbItemsUrl :: Text }

  deriving (Show, Generic)

instance FromJSON Env
instance ToJSON Env


-- | Provisional data structure as captured from the JSON object of variable items.
data JsonAv = JsonAv {itemCount :: Int}

  deriving (Show, Generic)

instance FromJSON JsonAv
instance ToJSON JsonAv
