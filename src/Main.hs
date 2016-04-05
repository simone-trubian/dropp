module Main where


import HttpDataSource
import HTML
import System.Environment (getArgs)
import Data.Text (pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import System.IO (stdout)
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
  ( sendEmail
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
    -- Read and parse all input files.
    args <- getArgs
    let [urlFile] = args

    urls <- readFile urlFile

    -- Fetch pages and construct response.
    pages <- getPages $ mapM getHTML (lines urls)

    -- Get timestamp.
    utcTime <- getCurrentTime

    -- Send email with AWS SES.
    let subText = pack $ "Avaliability " ++ generateTime utcTime

    env <- newEnv Ireland Discover
    logger <- newLogger Debug stdout
    _ <- runResourceT . runAWS (env & envLogger .~ logger) $
        send $ generateEmail subText $ toStrict $ decodeUtf8 $ formatOutput pages
    return ()


generateTime :: UTCTime -> String
generateTime utcTime = formatTime defaultTimeLocale format cestTime
  where
    cestTime = utcToLocalTime cest utcTime
    cest = TimeZone 120 True "CEST"
    format = "%a %d/%m/%Y %R"

-- ------------------------------------------------------------------------- --
--              AWS SES SERVICE
-- ------------------------------------------------------------------------- --

recipients = ["stoxx84@gmail.com", "simone.trubian@ondait.com"]


generateEmail subText payload = sendEmail "stoxx84@gmail.com" dest msg

    where
        dest = destination & dToAddresses .~ recipients
        msg = message subject body'
        subject = SES.content "" & cData .~ subText
        body' = body & bHTML .~ Just (SES.content payload)
