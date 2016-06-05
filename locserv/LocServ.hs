module Main where


import Dropp.DataTypes
import Dropp.HTML
import Lucid
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Data.Text.Internal (Text)
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


type TestAPI = "bangOK" :> Capture "title" Text :> Get '[HTMLLucid] Item
          :<|> "ebay" :> Capture "isOn" Bool :> Get '[HTMLLucid] EbayStatus
          :<|> "bangJSON" :> Capture "count" Int :> Get '[JSON] JsonAv


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance ToHtml Item where
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
server = bangOK
    :<|> ebay
    :<|> bangJSON

  where
    bangOK :: Text -> EitherT ServantErr IO Item
    bangOK title =
        return (Item "" "" title (Just "In stock, usually dispatched in 1 business day") Nothing)

    bangJSON :: Int -> EitherT ServantErr IO JsonAv
    bangJSON availability = return (JsonAv availability)

    ebay :: Bool -> EitherT ServantErr IO EbayStatus
    ebay isOn = return (if isOn then On else Off)


app :: Application
app = serve blockAPI server
