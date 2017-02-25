module Main where

import Hecate.Database (initDatabase)
import Hecate.IO (evalCommand, getHome)
import Hecate.IO.Parser (runCLIParser)
import Hecate.Types (AppContext (..), runAppM)
import System.Exit
import System.IO
import qualified Database.SQLite.Simple as SQLite

{-# ANN main "HLint: ignore Use print" #-}
main :: IO ()
main = do
  home       <- getHome >>= maybe (error "Can't find my way HOME") pure
  connection <- SQLite.open (home ++ "/.hecate/data.db")
  _          <- initDatabase connection
  cmd        <- runCLIParser
  resp       <- runAppM (AppContext (home ++ "/.hecate/auth.json") connection) (evalCommand cmd)
  case resp of
    Left e  -> hPrint stderr e >> exitFailure
    Right o -> hPrint stdout o >> exitSuccess
