{-# LANGUAGE ScopedTypeVariables #-}


module Dropp.Http
  ( getItemUpdate
  , getItems)

where


import Dropp.DataTypes
import Dropp.HTML
import Data.Text.Internal (Text)
import Safe (headMay)
import Data.Either (isRight)
import Data.Text (unpack)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as Bs (ByteString)
import Control.Exception.Lifted (catch)
import Control.Monad.Trans.Maybe
  ( MaybeT
  , runMaybeT)

import Data.Maybe
  ( fromJust
  , isJust)

import Control.Monad
  ( mzero
  , guard)


import Network.HTTP.Types.Header
  ( ResponseHeaders
  , Header
  , hContentType)

import Network.HTTP.Conduit
  ( Manager
  , Response
  , HttpException
  , httpLbs
  , responseHeaders
  , responseBody
  , parseUrl)

import Text.Parsec
  ( ParseError
  , string
  , parse)


type MaybeIO = MaybeT IO

getItemUpdate :: Manager -> Item -> IO Item
getItemUpdate mgr item =
    (updateItem item) <$>
    getAvailability mgr (source_url item) <*>
    getEbayStatus mgr (ebay_url item)


getItems :: Manager -> Text -> IO (Maybe [Item])
getItems mgr url =
    runMaybeT
    $ getHttp mgr url "application/json" decode


getAvailability :: Manager -> Text -> IO (Maybe Text)
getAvailability mgr url =
    runMaybeT
    $ getHttp mgr url "text/html" scrapeBGAv



getEbayStatus :: Manager -> Text -> IO (Maybe EbayStatus)
getEbayStatus mgr url =
    runMaybeT
    $ getHttp mgr url "text/html" scrapeEbayStatus


-- | I'm a keeper!
getHttp :: Manager -> Text -> String -> (ByteString -> Maybe a) -> MaybeIO a
getHttp mgr url contentType function = do
    res <- catch
        (function <$> fetchHttp mgr url contentType)
        (\(x :: HttpException)-> mzero)

    guard (isJust res)
    return $ fromJust res


fetchHttp :: Manager -> Text -> String -> MaybeIO ByteString
fetchHttp manager url contentType = do
    request <- parseUrl $ unpack url
    response <- (httpLbs request manager)

    let cType = getCType $ responseHeaders response

    guard (isRight $ cType contentType)
    return (responseBody response)


getContentType :: ResponseHeaders -> Maybe Bs.ByteString
getContentType headers = snd <$> cType headers
  where
    cType :: ResponseHeaders -> Maybe Header
    cType = headMay . filter (\header -> fst header == hContentType)


getCType :: ResponseHeaders -> String -> Either ParseError String
getCType headers cType = parse (string cType) "" $ fromJust $ getContentType headers
