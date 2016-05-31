module Main where


import Dropp.HttpDataSource
import Dropp.HTML
import Dropp.DataTypes
import qualified Data.ByteString as By
import Data.Yaml (decode)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Text.Internal (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import System.IO (stdout)
import Control.Applicative ((<$>))
import Data.Time.Clock
  ( UTCTime
  , getCurrentTime)

import Data.Time.LocalTime
  ( TimeZone (TimeZone)
  , utcToLocalTime)

import Data.Time.Format
  ( formatTime
  , defaultTimeLocale)

import Network.AWS
  ( Region (Ireland)
  , Credentials (Discover)
  , LogLevel (Debug)
  , newEnv
  , newLogger
  , envLogger
  , send
  , runAWS
  , runResourceT)

import qualified Network.AWS.SES as SES
import Network.AWS.SES
  ( SendEmail
  , sendEmail
  , dToAddresses
  , cData
  , bHTML
  , destination
  , body
  , message)

import Control.Lens
  ( (&)
  , (.~))


-- ------------------------------------------------------------------------- --
--              MAIN
-- ------------------------------------------------------------------------- --

main :: IO ()
main = do

    -- Read configuration file.
    [filePath] <- getArgs
    vars <- decode <$> By.readFile filePath :: IO (Maybe Env)
    let envVars = fromJust vars

    -- Fetch pages urls from DB.
    [dbItems] <- getPages $ mapM getItems [JsonUrl (dbUrls envVars)]

    let urls = map source_url $ fromJust dbItems

    -- Fetch all pages listed in the DB table.
    pages <- getPages $ mapM getHTML urls

    -- Get timestamp.
    utcTime <- getCurrentTime

    -- Generate email subject.
    let subText = pack $ "Availability " ++ formatTimeStamp utcTime

    -- Generate email HTML body.
    let bodyText = toStrict $ decodeUtf8 $ formatOutput pages

    -- Generate full report email.
    let email = makeEmail envVars subText bodyText

    -- Generate AWS environment and insantiate logger.
    env <- newEnv Ireland Discover
    logger <- newLogger Debug stdout

    -- Send report email.
    _ <- runResourceT . runAWS
        (env & envLogger .~ logger)
        $ send email

    return ()


-- | Generate a string containing a local timestamp in a human readable format.
formatTimeStamp :: UTCTime -> String
formatTimeStamp utcTime = formatTime defaultTimeLocale format cestTime
  where
    cestTime = utcToLocalTime cest utcTime
    cest = TimeZone 120 True "CEST"
    format = "%a %d/%m/%Y %R"

-- ------------------------------------------------------------------------- --
--              AWS SES SERVICE
-- ------------------------------------------------------------------------- --


-- | Generate a list of emails to be sent.
makeEmail :: Env -> Text -> Text -> SendEmail
makeEmail envVars subText payload = sendEmail (sender envVars) dest msg
  where
    dest = destination & dToAddresses .~ recipients envVars
    msg = message subject body'
    subject = SES.content "" & cData .~ subText
    body' = body & bHTML .~ Just (SES.content payload)
