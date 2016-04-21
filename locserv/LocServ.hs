module Main where


import Dropp.HTML
import Lucid
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Server
import Servant.HTML.Lucid
import Network.HTTP.Media
  ( (//)
  , (/:))


main :: IO ()
main = run 8081 app


block = ItemBlock "hello" "goodbye"


blockAPI :: Proxy BlockAPI
blockAPI = Proxy


server :: Server BlockAPI
server = return block


app :: Application
app = serve blockAPI server


type BlockAPI = "block" :> Get '[HTMLLucid] ItemBlock


data HTMLLucid


instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS


instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance ToHtml ItemBlock where
  toHtml block =
    tr_ $ do
      td_ (toHtml $ title block)
      td_ (toHtml $ availability block)


  toHtmlRaw = toHtml
