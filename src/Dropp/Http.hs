module Dropp.Http where


import Dropp.DataTypes
import Dropp.HTML
import Data.Text.Internal (Text)
import Safe (headMay)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as Bs (ByteString)
import Network.HTTP.Types.Header
  ( ResponseHeaders
  , Header
  , hContentType)

import Network.HTTP.Conduit
  ( Manager
  , Response
  , httpLbs
  , responseHeaders
  , responseBody
  , parseUrl)

import Text.Parsec
  ( ParseError
  , string
  , parse)


getItems :: Manager -> Text -> IO (Maybe [Item])
getItems manager url = do
    response <- fetchHttp manager url

    let cType = getCType $ responseHeaders response

    case cType "application/json" of
      Right _ -> return $ decode $ responseBody response
      Left _ -> return Nothing


getAvailability :: Manager -> Text -> IO (Maybe Text)
getAvailability manager url = do
    response <- fetchHttp manager url

    let cType = getCType $ responseHeaders response

    case cType "text/html" of
      Right _ -> return $ scrapeBGAv $ responseBody response
      Left _ -> return Nothing


getEbayStatus :: Manager -> Text -> IO (Maybe EbayStatus)
getEbayStatus manager url = do
    response <- fetchHttp manager url

    let cType = getCType $ responseHeaders response

    case cType "text/html" of
      Right _ -> return $ scrapeEbayStatus $ responseBody response
      Left _ -> return Nothing


fetchHttp :: Manager -> Text -> IO (Response ByteString)
fetchHttp manager url = do
    request <- parseUrl $ unpack url
    httpLbs request manager


getContentType :: ResponseHeaders -> Maybe Bs.ByteString
getContentType headers = snd <$> cType headers
  where
    cType :: ResponseHeaders -> Maybe Header
    cType = headMay . filter (\header -> fst header == hContentType)


getCType :: ResponseHeaders -> String -> Either ParseError String
getCType headers cType = parse (string cType) "" $ fromJust $ getContentType headers
