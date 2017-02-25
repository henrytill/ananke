module Main where

import Control.Exception
import Hecate.Server
import Hecate.Server.Database (initDatabase)
import Hecate.Server.Types
import Hecate.Util
import Network.Wai.Handler.Warp
import qualified Database.SQLite.Simple as SQLite

runServer :: SQLite.Connection -> IO ()
runServer connection = do
  initDatabase connection
  run 8081 . app $ ServerContext connection

main :: IO ()
main =
  getHome >>= maybe (error "Can't find my way HOME") pure >>= \home ->
  bracket
    (SQLite.open (home ++ "/.hecate/data.db"))
    runServer
    SQLite.close
