module Main where


import Dropp.DataTypes
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Data.Text.Internal (Text)
import Control.Monad.Trans.Either (EitherT)
import Servant
  ( Proxy (Proxy)
  , ServantErr)

import Lucid
  ( ToHtml
  , HtmlT
  , Html
  , toHtml
  , toHtmlRaw
  , renderBS
  , html_
  , title_
  , body_
  , div_
  , class_)

import Servant.API
  --( MimeRender
  --, Accept
  --, Get
  --, Capture
  --, JSON
  --, (:>)
  --, (:<|>)
  --, contentType
  --, mimeRender)

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


type TestAPI = "bangOK" :> Capture "title" Text :> Get '[HTMLLucid] ItemBlock
           :<|> "bangJSON" :> Capture "count" Int :> Get '[JSON] JsonAv

-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance ToHtml ItemBlock where
  toHtml block = bangGoodMockPage block

  toHtmlRaw = toHtml


bangGoodMockPage :: Monad m => ItemBlock -> HtmlT m ()
bangGoodMockPage block =
    html_ $ do
      title_ (toHtml $ title block)
      body_ (div_ [class_ "status"] (toHtml $ availability block))


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

main :: IO ()
main = run 8081 app


blockAPI :: Proxy TestAPI
blockAPI = Proxy


server :: Server TestAPI
server = bangOK :<|> bangJSON

  where
    bangOK :: Text -> EitherT ServantErr IO ItemBlock
    bangOK title =
        return (ItemBlock title "In stock, usually dispatched in 1 business day")

    bangJSON :: Int -> EitherT ServantErr IO JsonAv
    bangJSON availability = return (JsonAv availability)


app :: Application
app = serve blockAPI server
