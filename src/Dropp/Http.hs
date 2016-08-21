{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains all the Http fetching and manipulation functions.
-- The module uses the
-- <https://hackage.haskell.org/package/http-conduit-2.1.10.1 Http Conduit> libary.
module Dropp.Http
  ( getItemUpdate
  , getItems
  , writeItemSnapshot)

where


import Dropp.DataTypes
import Dropp.HTML()
import Safe (headMay)
import Text.Printf (printf)
import Control.Exception.Lifted (catch)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Text.Encoding (encodeUtf8)

import Data.Text
  ( unpack
  , pack
  , append
  , breakOn)

import Data.Aeson
  ( FromJSON
  , encode
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
  , Request
  , HttpException
  , RequestBody(RequestBodyLBS)
  , httpLbs
  , method
  , requestBody
  , queryString
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
    getAvailability mgr (sourceUrl item) <*>
    getEbayStatus mgr (ebayUrl item)



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



-- | Perform an HTTP GET request to fetch the snaphot of an item.
getSnapshot
    :: Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> IO (Maybe [Snapshot]) -- ^IO computation returning a Maybe Snapshot list.

getSnapshot mgr url = runMaybeT $ fetchHttp mgr url



-- | Perform the HTTP call with a method of choice.
writeItemSnapshot
    -- :: (ToJSON a) =>
    :: Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> Snapshot
    -> IO (Maybe LBS.ByteString)

writeItemSnapshot manager url payload = runMaybeT $ do
    let patchUrl = url `append` pack ("?id=eq." ++ show (snapItemId payload))
    initRequest <- parseUrl $ unpack patchUrl
    let reqBody = RequestBodyLBS $ encode payload
    let request = initRequest {method = "PATCH"} {requestBody = reqBody}
    response <- catch
        (httpLbs request manager)
        (\(_ :: HttpException)-> do
            initPostReq <- parseUrl $ unpack url
            let requestPost = initPostReq {method = "POST"} {requestBody = reqBody}
            catch
                (httpLbs requestPost manager)
                (\ (x :: HttpException)-> do
                    lift $ printf "failed to post %s because of %s\n"
                           (unpack url)
                           (show x)
                    mzero))

    return $ responseBody response



-- | Perform the HTTP call and return a response body if the content type
-- matches the one requested.
fetchHttp
    :: (FromJSON a, FromHTML a)
    => Manager -- ^Conduit HTTP manager.
    -> URL -- ^URL to be fetched.
    -> MaybeIO a -- ^Converted value wrapped in a MaybeT IO computation.

fetchHttp manager url = do
    -- Please note that this function uses the `parseBangUrl` function to
    -- overcome limitation of BangGood.
    request <- parseBangUrl url
    response <- catch
        (httpLbs request manager)
        (\(x :: HttpException)-> do
            lift $ printf "failed to fetch %s because of %s\n"
                   (unpack url)
                   (show x)
            mzero)

    -- Extract content type from the response and fail if not found.
    let responseCType = getContentType $ responseHeaders response
    guard (isJust responseCType)

    -- Check if the content types match and fail otherwise.
    let mimeType = getMimeType (parseContentType $ fromJust responseCType)
    guard (isJust mimeType)

    -- Return the wanted data type using the right decoding function.
    let decodedType = decodeHTML $ responseBody response
    case decodedType of
        Nothing -> do
          let decodedJSON = decode $ responseBody response
          guard (isJust decodedJSON)
          return $ fromJust decodedJSON

        _ -> return $ fromJust decodedType



-- | Extract content type from the list of header elements.
getContentType
    :: ResponseHeaders -- ^List of header elements of the response.
    -> Maybe BS.ByteString -- ^ Value of the content type if found.

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
    [_, Right _] -> Just ApplicationJson
    _ -> Nothing

  where
    parseResults :: [Either ParseError String]
    parseResults = map parseHeader ["text/html", "application/json"]



-- | Check if the content type of the response matches the one requested.
parseContentType
    :: BS.ByteString -- ^ Value of the content type found in the request.
    -> String -- ^Content type requested by the calling function.
    -> Either ParseError String -- ^Parsing result.

parseContentType responseCType demandedCType =
    parse (string demandedCType) "" responseCType


-- | Custom url parser used to overcome bad query strings used by BangGood.
-- BangGood uses badly formatted queries that are regected by the `parseUrl`
-- function. This function generates a request type only by using the domain
-- and slug part of the URL and then updates the request with the original query
-- string.
parseBangUrl
    :: MonadThrow m
    => URL -- ^ URL used to generate the request.
    -> m Request -- ^ Request updated with the original query string.

parseBangUrl url = do
    let brokenUrl = breakOn "?" url
    request <- parseUrl $ unpack (fst brokenUrl)
    return $ request {queryString = encodeUtf8 (snd brokenUrl)}
