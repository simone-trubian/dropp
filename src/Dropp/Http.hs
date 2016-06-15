{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains all the Http fetching and manipulation functions.
-- The module uses the
-- <https://hackage.haskell.org/package/http-conduit-2.1.10.1 Http Conduit> libary.
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
import Text.Printf (printf)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as Bs (ByteString)
import Control.Exception.Lifted (catch)
import Control.Monad.Trans (lift)
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


-- | Convenience alias of a Maybe transformer that wraps IO actions and
-- returns a value of some type.
type MaybeIO = MaybeT IO


-- | Update the source availabity and the ebay status for an item data record.
-- The function performs two HTTP calls using an applicative style.
getItemUpdate
    :: Manager -- ^Conduit HTTP manager.
    -> Item -- ^Item record to be updated.
    -> IO Item -- ^IO action returning an updated item record.

getItemUpdate mgr item =
    updateItem item <$>
    getAvailability mgr (source_url item) <*>
    getEbayStatus mgr (ebay_url item)



-- | Perform an HTTP GET request to fetch a list of items records.
getItems
    :: Manager -- ^Conduit HTTP manager.
    -> Text -- ^URL to be fetched.
    -> IO (Maybe [Item]) -- ^IO action containing a Maybe list of items.

getItems mgr url =
    runMaybeT
    $ getHttp mgr url "application/json" decode



-- | Perform an HTTP GET request to fetch the source availability of the item.
getAvailability
    :: Manager -- ^Conduit HTTP manager.
    -> Text -- ^URL to be fetched.
    -> IO (Maybe Text) -- ^IO computation returning a Maybe availabilty.

getAvailability mgr url =
    runMaybeT
    $ getHttp mgr url "text/html" scrapeBGAv



-- | Perform an HTTP GET request to fetch the source availability of the item.
getEbayStatus
    :: Manager -- ^Conduit HTTP manager.
    -> Text -- ^URL to be fetched.
    -> IO (Maybe EbayStatus) -- ^IO computation returning a Maybe Ebay status.

getEbayStatus mgr url =
    runMaybeT
    $ getHttp mgr url "text/html" scrapeEbayStatus



-- | Main polymorphic interface function called by all the exported API
-- function to perform the HTTP call and convert the returned reply body
-- into the desired data type.
getHttp
    :: Manager -- ^Conduit HTTP manager.
    -> Text -- ^URL to be fetched.
    -> String -- ^Content type required by the convesion function.
    -> (ByteString -> Maybe a) -- ^Conversion function.
    -> MaybeIO a -- ^Converted value wrapped in a MaybeT IO computation.

getHttp mgr url contentType function = do
    res <- catch
        (function <$> fetchHttp mgr url contentType)
        (\(x :: HttpException)-> do
            lift $ printf "failed to fetch %s because of %s\n" (unpack url) (show x)
            mzero)

    guard (isJust res)
    return $ fromJust res



-- | Perform the HTTP call and return a response body if the content type
-- matches the one requested.
fetchHttp
    :: Manager -- ^Conduit HTTP manager.
    -> Text -- ^URL to be fetched.
    -> String -- ^Content type string.
    -> MaybeIO ByteString -- ^Request body response wrapped in a MaybeT IO.

fetchHttp manager url demandedCType = do
    request <- parseUrl $ unpack url
    response <- httpLbs request manager

    -- Extract content type from the response and fail if not found.
    let responseCType = getContentType $ responseHeaders response
    guard (isJust responseCType)

    -- Check if the content types match and fail otherwise.
    let parsedCType = parseContentType (fromJust responseCType) demandedCType
    guard (isRight parsedCType)

    return (responseBody response)



-- | Check if the content type of the response matches the one requested.
parseContentType
    :: Bs.ByteString -- ^ Value of the content type found in the request.
    -> String -- ^Content type requested by the calling function.
    -> Either ParseError String -- ^Parsing result.

parseContentType responseCType demandedCType =
    parse (string demandedCType) "" responseCType



-- | Extract content type from the list of header elements.
getContentType
    :: ResponseHeaders -- ^List of header elements of the response.
    -> Maybe Bs.ByteString -- ^ Value of the content type if found.

getContentType headers = snd <$> cType headers
  where
    cType :: ResponseHeaders -> Maybe Header
    cType = headMay . filter (\header -> fst header == hContentType)
