{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


-- |This module implements HTTP requests using the
-- <https://github.com/facebook/Haxl Haxl library>. As of the current version
-- implements only the concrete type 'GetHTML' which is used to fetch HTML
-- pages.
module Dropp.HttpDataSource
  ( HttpException
  , URL (..)
  , getHTML
  , getPages
  , initDataSource)
  where


import Data.Typeable
import Control.Monad (void)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Data.Aeson (
    FromJSON
  , ToJSON
  , decode)

import Data.Hashable
  ( Hashable
  , hashWithSalt
  , hash)

import Haxl.Core
  ( StateKey
  , DataSource
  , DataSourceName
  , State
  , Show1
  , GenHaxl
  , BlockedFetch (BlockedFetch)
  , PerformFetch (SyncFetch)
  , initEnv
  , stateSet
  , stateEmpty
  , runHaxl
  , show1
  , dataSourceName
  , dataFetch
  , putFailure
  , putSuccess
  , fetch)

import Control.Exception
  ( SomeException
  , try)

import Network.HTTP.Conduit
  ( Manager
  , HttpException
  , newManager
  , tlsManagerSettings
  , parseUrl
  , responseBody
  , httpLbs)


-- ------------------------------------------------------------------------- --
--              TYPES
-- ------------------------------------------------------------------------- --

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

-- deriving instance Typeable URL


-- |Json object returned by the /urls endpoint.
data Urls = Url {url :: String}

  deriving (Show, Generic)

instance FromJSON Urls
instance ToJSON Urls


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

-- |Haxl GADT
data HttpReq a where
    -- HTML fetching concrete type
    GetHTML
        :: URL --  URL literal to be fetched.
        -> HttpReq ByteString --  HTML bytestring boxed in a Haxl fetch.

    GetJSON
        :: URL --  URL literal to be fetched.
           -> HttpReq (Maybe Urls) --  Aeson JSON boxed in a Haxl fetch.

deriving instance Show (HttpReq a)

deriving instance Typeable HttpReq

instance Show1 HttpReq where show1 = show

deriving instance Eq (HttpReq a)

instance Hashable (HttpReq a) where
    hashWithSalt salt (GetHTML (url :: URL)) =
        hashWithSalt salt (0 :: Int, url :: URL)

    hashWithSalt salt (GetJSON (url :: URL)) =
        hashWithSalt salt (1 :: Int, url :: URL)

instance DataSourceName HttpReq where
    dataSourceName _ = "HttpDataSource"

instance StateKey HttpReq where
    data State HttpReq = HttpState Manager


-- ------------------------------------------------------------------------- --
--              IMPLEMENTATION
-- ------------------------------------------------------------------------- --

-- | Initialise the HTTP data source with a blank conduit manager.
initDataSource :: IO (State HttpReq)
initDataSource = HttpState <$> newManager tlsManagerSettings


instance DataSource u HttpReq where
  fetch (HttpState mgr) _flags _userEnv blockedFetches =
      SyncFetch $ mapM_ (fetchURL mgr) blockedFetches


-- |Perform HTTP requests of HTML URL's. All requests are intended to be GET
-- and the server will return an HTML document.
fetchURL
    :: Manager -- ^ Initalised Conduit manager.
    -> BlockedFetch HttpReq -- ^ Accepts any concrete type of the HttpReq GADT.
    -> IO ()

fetchURL mgr (BlockedFetch (GetHTML (HtmlUrl url)) var) = do
    printf $ "Fetching " ++ show url ++ "\n" -- FIXME: log instead of print.
    threadDelay 1000000
    fetchedHtml <- try $ do
        req <- parseUrl $ show url
        responseBody <$> httpLbs req mgr

    either (putFailure var) (putSuccess var)
        (fetchedHtml :: Either SomeException ByteString)


fetchURL mgr (BlockedFetch (GetJSON (JsonUrl url)) var) = do
    printf $ "Fetching " ++ show url ++ "\n" -- FIXME: log instead of print.
    threadDelay 1000000
    fetchedJson <- try $ do
        req <- parseUrl $ show url
        body <- responseBody <$> httpLbs req mgr
        return (decode body)

    either (putFailure var) (putSuccess var)
        (fetchedJson :: Either SomeException (Maybe Urls))


-- ------------------------------------------------------------------------- --
--              REQUESTS
-- ------------------------------------------------------------------------- --

-- |Fetch data for the concrete GetHTML HttpReq type.
getHTML
    :: URL -- ^ HTML url to be fetched.
    -> GenHaxl u ByteString -- ^ Haxl fetch.

getHTML = dataFetch . GetHTML


-- |Perform the Haxl data fetching.
getPages
    :: Haxl a -- ^ Any HttpReq concrete type composed to its fetch method.
    -> IO a -- ^ Return the concrete HttpReq return type.

getPages fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches
