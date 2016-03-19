module Main where


import BangDataSource
import Haxl.Core
  ( GenHaxl
  , initEnv
  , stateSet
  , stateEmpty
  , runHaxl)


type Haxl a = GenHaxl () a


getAvailability :: Haxl a -> IO a
getAvailability fetches = do
    dataSource <- initDataSource
    environment <- initEnv (stateSet dataSource stateEmpty) ()
    runHaxl environment fetches


urls = ["http://google.com"]


main :: IO ()
main = do
    page <- getAvailability $ mapM getHTML urls
    print page
