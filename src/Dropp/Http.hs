module Dropp.Http where


import Dropp.DataTypes
import Dropp.HTML
import Data.Text.Internal (Text)
import Safe (headMay)
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


getAvailability :: Text -> Maybe Text
getAvailability = undefined


getEbayStatus :: Manager -> Text -> IO (Maybe EbayStatus)
getEbayStatus manager url = do
    response <- fetchHttp manager url

    case (getContentType $ responseHeaders response) of
      Just "text/html" -> return $ scrapeEbayStatus $ responseBody response
      Just "application/json" -> return $ decode $ responseBody response
      Nothing -> return $ scrapeEbayStatus $ responseBody response


fetchHttp :: Manager -> Text -> IO (Response ByteString)
fetchHttp manager url = do
    request <- parseUrl $ unpack url
    httpLbs request manager


getContentType :: ResponseHeaders -> Maybe Bs.ByteString
getContentType headers = snd <$> cType headers
  where
    cType :: ResponseHeaders -> Maybe Header
    cType = headMay . filter (\header -> fst header == hContentType)
