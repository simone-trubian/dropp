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
  , serve)

import Network.HTTP.Media
  ( (//)
  , (/:))


-- ------------------------------------------------------------------------- --
--              API DEFINITION
-- ------------------------------------------------------------------------- --

data HTMLLucid


type TestAPI = "bangHTML" :> Capture "availability" Int :> Get '[HTMLLucid] Availability
          :<|> "ebay" :> Capture "isOn" Bool :> Get '[HTMLLucid] EbayStatus
          :<|> "bangJSON" :> Capture "count" Int :> Get '[JSON] JsonAv


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
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
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

main :: IO ()
main = run 8081 app


blockAPI :: Proxy TestAPI
blockAPI = Proxy


server :: Server TestAPI
server = bangHTML
    :<|> ebay
    :<|> bangJSON

  where
    bangHTML :: Int -> EitherT ServantErr IO Availability
    bangHTML av
      | av >= 10 = return Available
      | av >= 5 && av < 10 = return (AvCount av)
      | av < 5 && av > 0 = return (Low av)
      | otherwise = return Out

    bangJSON :: Int -> EitherT ServantErr IO JsonAv
    bangJSON av = return (JsonAv av)

    ebay :: Bool -> EitherT ServantErr IO EbayStatus
    ebay isOn = return (if isOn then On else Off)


app :: Application
app = serve blockAPI server
