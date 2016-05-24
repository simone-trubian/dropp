module Main where


import Dropp.DataTypes
import Dropp.HTML
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

import qualified Lucid as L

-- ------------------------------------------------------------------------- --
--              API DEFINITION
-- ------------------------------------------------------------------------- --

data HTMLLucid


type TestAPI = "bangOK" :> Capture "title" Text :> Get '[HTMLLucid] ItemBlock
           :<|> "bangJSON" :> Capture "count" Int :> Get '[JSON] JsonAv


-- ------------------------------------------------------------------------- --
--              BOILERPLATE
-- ------------------------------------------------------------------------- --

instance L.ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = L.renderBS . L.toHtml


instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance L.ToHtml ItemBlock where
  toHtml = bangGoodMockPage

  toHtmlRaw = L.toHtml


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
