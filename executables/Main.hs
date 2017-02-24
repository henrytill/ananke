module Main where

import Hecate.Server
import Hecate.Server.Database (initDatabase)
import Hecate.Server.Types
import Hecate.Util
import Network.Wai.Handler.Warp
import qualified Database.SQLite.Simple as SQLite

main :: IO ()
main = do
  home       <- getHome >>= maybe (error "Can't find my way HOME") pure
  connection <- SQLite.open (home ++ "/.hecate/data.db")
  _          <- initDatabase connection
  run 8081 . app $ ServerContext connection
