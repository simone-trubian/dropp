module Main where


import Dropp.HTML (ItemBlock (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy (Proxy))
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
  ( MimeRender
  , Accept
  , Get
  , (:>)
  , contentType
  , mimeRender)

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


type BlockAPI = "bangOK" :> Get '[HTMLLucid] ItemBlock


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


block = ItemBlock "Title" "In stock, usually dispatched in 1 business day"


blockAPI :: Proxy BlockAPI
blockAPI = Proxy


server :: Server BlockAPI
server = return block


app :: Application
app = serve blockAPI server
