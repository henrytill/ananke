module Main where

import Hecate.Server
import Hecate.Server.Types
import Network.Wai.Handler.Warp
import qualified Database.SQLite.Simple as SQLite

main :: IO ()
main = do
  connection <- SQLite.open "/tmp/hecate-tests/test.db"
  run 8081 . app $ ServerContext connection
