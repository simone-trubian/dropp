{-# LANGUAGE DeriveGeneric #-}


module Dropp.DataTypes where


import Haxl.Core (GenHaxl)
import GHC.Generics (Generic)
import Data.Text.Internal (Text)
import Data.Text (pack)
import Data.Hashable
  ( Hashable
  , hash)

import Data.Aeson
  ( FromJSON
  , ToJSON)


-- |Convenience alias for a GenHaxl with no user crentials.
type Haxl a = GenHaxl () a


-- |URL literal.
data URL =
    HtmlUrl String --  Url pointing to an html page.
  | JsonUrl String --  Url pointing to a JSON endpoint.

  deriving (Show, Ord, Eq, Generic)

instance Hashable URL where
    hash (HtmlUrl url) = hash $ show url
    hash (JsonUrl url) = hash $ show url


-- |Unbox the payload in the URL type and convert it to Byte String.
urlToText :: URL -> Text
urlToText (HtmlUrl url) = pack url
urlToText (JsonUrl url) = pack url


-- |Json object returned by the /urls endpoint.
data Urls = Url {url :: String}

  deriving (Show, Generic)

instance FromJSON Urls
instance ToJSON Urls


-- | Contains the data regarding a single item as it is parsed from the scraped
-- from the provider website.
data ItemBlock = ItemBlock
  { -- |Long-hand name of the item as it parsed from the title of its BangGood
    -- web page.
    title :: Text
    -- |Long-hand availability as it is parsed from the status div of  its
    -- BangGood web page.
  , availability :: Text}


instance Show ItemBlock where
    show (ItemBlock title availability) =
        show title ++ "\n" ++ show availability


-- | Provisional data structure as captured from the JSON object of variable items.
data JsonAv = JsonAv {itemCount :: Int}

  deriving (Show, Generic)

instance FromJSON JsonAv
instance ToJSON JsonAv


-- | Environment variables data type.
data Env = Env
  { recipients :: [Text]
  , sender :: Text
  , dbUrls :: String }

  deriving (Show, Generic)

instance FromJSON Env
instance ToJSON Env


-- | Type alias for an ebay status.
data EbayStatus = EbayStatus Bool
