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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as Bs (ByteString)
import Control.Exception.Lifted (catch)
import Control.Monad.Trans (lift)
import Data.Aeson
  ( FromJSON
  , decode)

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
    -> URL -- ^URL to be fetched.
    -> IO (Maybe [Item]) -- ^IO action containing a Maybe list of items.

getItems mgr url = runMaybeT $ fetchHttp mgr url



-- | Perform an HTTP GET request to fetch the source availability of the item.
getAvailability
    :: Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> IO (Maybe Availability) -- ^IO computation returning a Maybe availabilty.

getAvailability mgr url = runMaybeT $ fetchHttp mgr url



-- | Perform an HTTP GET request to fetch the source availability of the item.
getEbayStatus
    :: Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> IO (Maybe EbayStatus) -- ^IO computation returning a Maybe Ebay status.

getEbayStatus mgr url = runMaybeT $ fetchHttp mgr url



-- | Perform the HTTP call and return a response body if the content type
-- matches the one requested.
fetchHttp
    :: (FromJSON a, FromHTML a)
    => Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> MaybeIO a -- ^Converted value wrapped in a MaybeT IO computation.

fetchHttp manager url = do
    request <- parseUrl $ unpack url
    response <- catch
        (httpLbs request manager)
        (\(x :: HttpException)-> do
            lift $ printf "failed to fetch %s because of %s\n" (unpack url) (show x)
            mzero)

    -- Extract content type from the response and fail if not found.
    let responseCType = getContentType $ responseHeaders response
    guard (isJust responseCType)

    -- Check if the content types match and fail otherwise.
    let mimeType = getMimeType (parseContentType $ fromJust responseCType)
    guard (isJust mimeType)

    -- Return the wanted data type using the right decoding function.
    case fromJust mimeType of
       TextHtml -> do
           let decodedType = decodeHTML $ responseBody response
           guard (isJust decodedType)
           return $ fromJust decodedType

       _ -> do
           let decodedType = decode $ responseBody response
           guard (isJust decodedType)
           return $ fromJust decodedType



-- | Extract content type from the list of header elements.
getContentType
    :: ResponseHeaders -- ^List of header elements of the response.
    -> Maybe Bs.ByteString -- ^ Value of the content type if found.

getContentType headers = snd <$> cType headers
  where
    cType :: ResponseHeaders -> Maybe Header
    cType = headMay . filter (\header -> fst header == hContentType)



-- | Extract content type from the list of header elements.
getMimeType
    -- | Function that maps a mime type to its parsed value.
    :: (String -> Either ParseError String)
    -> Maybe MimeType -- ^ Value of the content type if found.

getMimeType parseHeader =
  case parseResults of
    (Right _ : _) -> Just TextHtml
    [_, Right _, _] -> Just ApplicationJson
    _ -> Nothing

  where
    parseResults :: [Either ParseError String]
    parseResults = map parseHeader ["text/html", "application/json"]



-- | Check if the content type of the response matches the one requested.
parseContentType
    :: Bs.ByteString -- ^ Value of the content type found in the request.
    -> String -- ^Content type requested by the calling function.
    -> Either ParseError String -- ^Parsing result.

parseContentType responseCType demandedCType =
    parse (string demandedCType) "" responseCType
