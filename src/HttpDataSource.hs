{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module HttpDataSource
  ( HttpException
  , getHTML
  , initDataSource
  ) where


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

type URL = String


type HTML = ByteString


data HttpReq a where
  GetHTML :: URL -> HttpReq HTML

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

initDataSource :: IO (State HttpReq)
initDataSource = HttpState <$> newManager tlsManagerSettings


-- Why `void`?
-- `void $ mapM ..` equals to `mapM_` that is it discards the results of an IO
-- action. The reason why it is used here is that there is no version of
-- `mapConcurrently` that does that.
instance DataSource u HttpReq where
  fetch (HttpState mgr) _flags _userEnv blockedFetches =
    SyncFetch $ void $ mapConcurrently (fetchURL mgr) blockedFetches


fetchURL :: Manager -> BlockedFetch HttpReq -> IO ()
fetchURL mgr (BlockedFetch (GetHTML url) var) = do
    printf "Fetching url.\n"
    threadDelay 1000000
    e <- try $ do
        req <- parseUrl url
        responseBody <$> httpLbs req mgr

    either (putFailure var) (putSuccess var)
        (e :: Either SomeException HTML)


-- ------------------------------------------------------------------------- --
--              REQUESTS
-- ------------------------------------------------------------------------- --

getHTML :: URL -> GenHaxl u HTML
getHTML = dataFetch . GetHTML
