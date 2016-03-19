{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module BangDataSource
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


data BangReq a where
  GetHTML :: URL -> BangReq HTML

deriving instance Show (BangReq a)

deriving instance Typeable BangReq

instance Show1 BangReq where show1 = show

deriving instance Eq (BangReq a)

instance Hashable (BangReq a) where
  hashWithSalt salt (GetHTML url) = hashWithSalt salt (0 :: Int, url)

instance DataSourceName BangReq where
  dataSourceName _ = "BangGoodDataSource"

instance StateKey BangReq where
  data State BangReq = HttpState Manager


-- ------------------------------------------------------------------------- --
--              IMPLEMENTATION
-- ------------------------------------------------------------------------- --

initDataSource :: IO (State BangReq)
initDataSource = HttpState <$> newManager tlsManagerSettings


-- Why `void`?
-- `void $ mapM ..` equals to `mapM_` that is it discards the results of an IO
-- action. The reason why it is used here is that there is no version of
-- `mapConcurrently` that does that.
instance DataSource u BangReq where
  fetch (HttpState mgr) _flags _userEnv blockedFetches =
    SyncFetch $ void $ mapConcurrently (fetchURL mgr) blockedFetches


fetchURL :: Manager -> BlockedFetch BangReq -> IO ()
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
