module Main where


import HttpDataSource
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime)
import Text.HTML.DOM (parseLBS)
import Data.Text.Internal (Text)
import Data.Text (pack)
import System.IO (stdout)
import Data.ByteString.Lazy.Internal (ByteString)

import Haxl.Core
  ( GenHaxl
  , initEnv
  , stateSet
  , stateEmpty
  , runHaxl)

import Text.XML.Cursor
  ( ($//)
  , ($|)
  , (>=>)
  , attributeIs
  , fromDocument
  , element
  , content
  , child)

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
  , bText
  , destination
  , body
  , message)

import Control.Lens
  ( (&)
  , (.~))


-- ------------------------------------------------------------------------- --
--              TYPES
-- ------------------------------------------------------------------------- --

type Haxl a = GenHaxl () a


data Email = Email
  { title :: Text
  , availability :: Text}

instance Show Email where
    show (Email title availability) =
        show title ++ "\n" ++ show availability ++ "\n"


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

    -- Send email with AWS SES.
    now <- getCurrentTime
    let subText = pack $ "Avaliability " ++ show now

    env <- newEnv Ireland Discover
    logger <- newLogger Debug stdout
    _ <- runResourceT . runAWS (env & envLogger .~ logger) $
        send $ generateEmail subText $  formatOutput pages
    return ()


-- ------------------------------------------------------------------------- --
--              AWS SES SERVICE
-- ------------------------------------------------------------------------- --

recipients = ["stoxx84@gmail.com", "simone.trubian@ondait.com"]


generateEmail subText payload = sendEmail "stoxx84@gmail.com" dest msg

    where
        dest = destination & dToAddresses .~ recipients
        msg = message subject body'
        subject = SES.content "" & cData .~ subText
        body' = body & bText .~ Just (SES.content payload)


-- ------------------------------------------------------------------------- --
--              RESPONSE PARSING
-- ------------------------------------------------------------------------- --

parseTitle cursor =
    head . content . head $
    cursor $//
    element "title" >=>
    child


parseAvailability cursor =
    head . content . head $
    cursor $//
    element "div" >=>
    attributeIs "class" "status" >=>
    child


makeBlock page = Email getTitle getAvailability
    where
        getTitle = parseTitle $ (fromDocument . parseLBS) page
        getAvailability = parseAvailability $ (fromDocument . parseLBS) page


getTitleNode = parseTitle . fromDocument . parseLBS


getAvailabilityNode = parseAvailability . fromDocument . parseLBS


formatOutput :: [ByteString] -> Text
formatOutput = pack . concatMap ((++"\n") . show . makeBlock)


getPages :: Haxl a -> IO a
getPages fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches
