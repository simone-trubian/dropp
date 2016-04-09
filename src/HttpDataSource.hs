{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- |This module implements HTTP requests using the
-- <https://github.com/facebook/Haxl Haxl library>. As of the current version
-- implements only the concrete type 'GetHTML' which is used to fetch HTML
-- pages.
module HttpDataSource
  ( HttpException
  , getHTML
  , getPages
  , initDataSource)
  where


import Data.Typeable
import Control.Monad (void)
import Text.Printf (printf)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy (ByteString)

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
--              TYPES
-- ------------------------------------------------------------------------- --

-- |Convenience alias for a GenHaxl with no user crentials.
type Haxl a = GenHaxl () a


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

-- |URL literal.
type URL = String


-- |Haxl GADT
data HttpReq a where
    -- HTML fetching concrete type
    GetHTML
        :: URL --  URL literal to be fetched.
        -> HttpReq ByteString --  HTML bytestring boxed in a Haxl fetch.

deriving instance Show (HttpReq a)

deriving instance Typeable HttpReq

instance Show1 HttpReq where show1 = show

deriving instance Eq (HttpReq a)

instance Hashable (HttpReq a) where
    hashWithSalt salt (GetHTML url) = hashWithSalt salt (0 :: Int, url)

instance DataSourceName HttpReq where
    dataSourceName _ = "HttpGoodDataSource"

instance StateKey HttpReq where
    data State HttpReq = HttpState Manager


-- ------------------------------------------------------------------------- --
--              IMPLEMENTATION
-- ------------------------------------------------------------------------- --

-- | Initialise the HTTP data source with a blank conduit manager.
initDataSource :: IO (State HttpReq)
initDataSource = HttpState <$> newManager tlsManagerSettings


-- Why `void`?
-- `void $ mapM ..` equals to `mapM_` that is it discards the results of an IO
-- action. The reason why it is used here is that there is no version of
-- `mapConcurrently` that does that.
instance DataSource u HttpReq where
  fetch (HttpState mgr) _flags _userEnv blockedFetches =
    SyncFetch $ void $ mapConcurrently (fetchURL mgr) blockedFetches


-- |Perform HTTP requests of HTML URL's. All requests are intended to be GET
-- and the server will return an HTML document.
fetchURL
    :: Manager -- ^ Initalised Conduit manager.
    -> BlockedFetch HttpReq -- ^ Accepts any concrete type of the HttpReq GADT.
    -> IO ()

fetchURL mgr (BlockedFetch (GetHTML url) var) = do
    printf "Fetching url.\n" -- FIXME: move to fetch method, log instead of print.
    threadDelay 1000000
    e <- try $ do
        req <- parseUrl url
        responseBody <$> httpLbs req mgr

    either (putFailure var) (putSuccess var)
        (e :: Either SomeException ByteString)


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
