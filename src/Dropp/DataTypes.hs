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


-- |Convenience alias for a GenHaxl with no user crentials.
type Haxl a = GenHaxl () a


-- ------------------------------------------------------------------------- --
--              URL
-- ------------------------------------------------------------------------- --

-- |URL literal.
data URL =
    HtmlUrl String --  Url pointing to an html page.
  | JsonUrl String --  Url pointing to a JSON endpoint.

  deriving (Show, Ord, Eq, Generic)

instance Hashable URL where
    hash (HtmlUrl url) = hash $ show url
    hash (JsonUrl url) = hash $ show url

instance FromJSON URL where
    parseJSON (String s) = case takeEnd 4 s of
        "html" -> pure (HtmlUrl $ unpack s)
        otherwise ->  pure (JsonUrl $ unpack s)

    parseJSON _ = empty

instance ToJSON URL

-- |Unbox the payload in the URL type and convert it to Text.
urlToText :: URL -> Text
urlToText (HtmlUrl url) = pack url
urlToText (JsonUrl url) = pack url



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


-- | Provisional data structure as captured from the JSON object of variable items.
data JsonAv = JsonAv {itemCount :: Int}

  deriving (Show, Generic)

instance FromJSON JsonAv
instance ToJSON JsonAv


updateItem :: Item -> Maybe Text -> Maybe EbayStatus -> Item
updateItem item availability ebayStatus = newItem
  where
    newItem = item {availability = availability}
    partialItem = item {onEbay = ebayStatus}


-- ------------------------------------------------------------------------- --
--              ENVIRONMENT
-- ------------------------------------------------------------------------- --

-- | Environment variables data type.
data Env = Env
  { recipients :: [Text]
  , sender :: Text
  , dbUrls :: String }

  deriving (Show, Generic)

instance FromJSON Env
instance ToJSON Env
