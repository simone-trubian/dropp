module Main where


--import BangDataSource
--import Network.AWS.SES.ListVerifiedEmailAddresses
--  ( listVerifiedEmailAddresses
--  , lvearsVerifiedEmailAddresses)
--import Network.AWS.SES (sES)
--import Network.AWS.Data
--import System.Environment (getArgs)
--import Data.Time.Clock (getCurrentTime)
--import Text.HTML.DOM (parseLBS)
--import Data.Text.Internal (Text)
--import Haxl.Core
--  ( GenHaxl
--  , initEnv
--  , stateSet
--  , stateEmpty
--  , runHaxl)
--
--import Text.XML.Cursor
--  ( ($//)
--  , ($|)
--  , (>=>)
--  , attributeIs
--  , fromDocument
--  , element
--  , content
--  , child)










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

import Network.AWS.SES
  ( sendEmail
  , dToAddresses
  , cData
  , bText
  , destination
  , content
  , body
  , message)

import Control.Lens
  ( (&)
  , (.~))

import System.IO (stdout)


testCreateSendMail = sendEmail "stoxx84@gmail.com" dest msg
    where
        dest = destination & dToAddresses .~ recipients
        msg = message content' body'
        content' = content "" & cData .~ "Availability "
        body' = body & bText .~ (Just (content ""))

testSendEmail :: IO ()
testSendEmail = do
    env <- newEnv Ireland Discover
    l <- newLogger Debug stdout
    _ <- runResourceT . runAWS (env & envLogger .~ l) $ (send testCreateSendMail)
    return ()


main = undefined













--type Haxl a = GenHaxl () a
--
--
--data Email = Email
--  { title :: Text
--  , availability :: Text}
--
--instance Show Email where
--    show (Email title availability) = show title ++ "\n" ++ show availability
--
--
--main :: IO ()
--main = do
--    -- Read and parse all input files.
--    args <- getArgs
--    let [urlFile, awsUser] = args
--
--    urls <- readFile urlFile
--
--    -- Fetch pages and construct response.
--    pages <- getPages $ mapM getHTML (lines urls)
--    now <- getCurrentTime
--    let formatOutput = (concatMap (++"\n")) . (map show) . map makeBlock
--    print $ (show now) ++ "\n" ++ formatOutput pages
--
--
--parseTitle cursor =
--    head . content . head $
--    cursor $//
--    element "title" >=>
--    child
--
--
--parseAvailability cursor =
--    head . content . head $
--    cursor $//
--    element "div" >=>
--    attributeIs "class" "status" >=>
--    child
--
--
--makeBlock page = Email (getTitle) (getAvailability)
--
--    where
--        getTitle = parseTitle $ (fromDocument . parseLBS) page
--        getAvailability = parseAvailability $ (fromDocument . parseLBS) page
--
--getTitleNode = parseTitle . (fromDocument . parseLBS)
--
--
--getAvailabilityNode = parseAvailability . (fromDocument . parseLBS)
--
--
--getPages :: Haxl a -> IO a
--getPages fetches = do
--    dataSource <- initDataSource
--    environment <- initEnv (stateSet dataSource stateEmpty) ()
--    runHaxl environment fetches
--
recipients = ["stoxx84@gmail.com", "simone.trubian@ondait.com"]
