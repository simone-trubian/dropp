{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- |This module implements HTTP requests using the
-- <https://github.com/facebook/Haxl Haxl library>. As of the current version
-- implements only the concrete type 'GetHTML' which is used to fetch HTML
-- pages.
module Dropp.HttpDataSource
  ( HttpException
  , URL (..)
  , Urls (..)
  , getHTML
  , getUrls
  , getPages
  , initDataSource)
  where


import Data.Typeable
import Dropp.DataTypes
import GHC.Exception (Exception)
import Control.Monad (void)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (decode)

import Data.Hashable
  ( Hashable
  , hashWithSalt)

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
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

-- |Haxl GADT
data HttpReq a where
    -- HTML fetching concrete type
    GetHTML
        :: URL --  URL literal to be fetched.
        -> HttpReq ByteString --  HTML bytestring boxed in a Haxl fetch.

    GetUrls
        :: URL --  URL literal to be fetched.
           -> HttpReq (Maybe [Urls]) --  Aeson JSON boxed in a Haxl fetch.

deriving instance Show (HttpReq a)

deriving instance Typeable HttpReq

instance Show1 HttpReq where show1 = show

deriving instance Eq (HttpReq a)

instance Hashable (HttpReq a) where
    hashWithSalt salt (GetHTML (url :: URL)) =
        hashWithSalt salt (0 :: Int, url :: URL)

    hashWithSalt salt (GetUrls (url :: URL)) =
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

fetchURL mgr (BlockedFetch (GetHTML (HtmlUrl url)) var) =
    fetchHttp mgr url >>= either (putFailure var) (putSuccess var)


fetchURL mgr (BlockedFetch (GetUrls (JsonUrl url)) var) =
    fetchHttp mgr url >>= \bytes -> either (putFailure var) (putSuccess var)
        (decode <$> bytes)


fetchHttp
    :: Manager -- ^ Initalised Conduit manager.
    -> String -- ^ Url to be fetched
    -> IO (Either SomeException ByteString)
fetchHttp mgr url = do
    printf $ "Fetching " ++ show url ++ "\n" -- FIXME: log instead of print.
    threadDelay 1000000
    fetchedHtml <- try $ do
        req <- parseUrl $ url
        responseBody <$> httpLbs req mgr

    return fetchedHtml


-- ------------------------------------------------------------------------- --
--              REQUESTS
-- ------------------------------------------------------------------------- --

-- |Fetch data for the concrete GetHTML HttpReq type.
getHTML
    :: URL -- ^ HTML url to be fetched.
    -> GenHaxl u ByteString -- ^ Haxl fetch.

getHTML = dataFetch . GetHTML


getUrls
    :: URL -- ^ HTML url to be fetched.
    -> GenHaxl u (Maybe [Urls]) -- ^ Haxl fetch.

getUrls = dataFetch . GetUrls


-- |Perform the Haxl data fetching.
getPages
    :: Haxl a -- ^ Any HttpReq concrete type composed to its fetch method.
    -> IO a -- ^ Return the concrete HttpReq return type.

getPages fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches
