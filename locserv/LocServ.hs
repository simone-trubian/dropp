module Main where


import Dropp.DataTypes
import Dropp.HTML
import Lucid
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Control.Monad.Trans.Either (EitherT)
import Servant.API
import Servant
  ( Proxy (Proxy)
  , ServantErr)

import Servant.Server
  ( Server
  , Handler
  , serve)

import Network.HTTP.Media
  ( (//)
  , (/:))


-- ------------------------------------------------------------------------- --
--              API DEFINITION
-- ------------------------------------------------------------------------- --

data HTMLLucid


type TestAPI = "bangHTML"
               :> Capture "availabilityCount" Int
               :> Get '[HTMLLucid] Availability

          :<|> "bangJSON"
                :> Capture "availabilityCount" Int
                :> Get '[JSON] Availability

          :<|> "ebay"
               :> Capture "isOn" Bool
               :> Get '[HTMLLucid] EbayStatus


-- ------------------------------------------------------------------------- --
--              HTML CLASSES IMPLEMENTATION
-- ------------------------------------------------------------------------- --

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance ToHtml Availability where
  toHtml = bangGoodMockPage

  toHtmlRaw = toHtml


instance ToHtml EbayStatus where
  toHtml = ebayMockPage

  toHtmlRaw = toHtml


-- ------------------------------------------------------------------------- --
--              SERVER DEFINITION
-- ------------------------------------------------------------------------- --

main :: IO ()
main = run 8081 app


blockAPI :: Proxy TestAPI
blockAPI = Proxy


server :: Server TestAPI
server = bangAv
    :<|> bangAv
    :<|> ebayStat

  where
    bangAv :: Int -> Handler Availability
    bangAv av
      | av >= 10 = return Available
      | av >= 5 && av < 10 = return (AvCount av)
      | av < 5 && av > 0 = return (Low av)
      | otherwise = return Out

    ebayStat :: Bool -> Handler EbayStatus
    ebayStat isOn = return (if isOn then On else Off)


app :: Application
app = serve blockAPI server
