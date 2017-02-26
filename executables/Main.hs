module Main where

import Control.Exception
import Hecate.Database (initDatabase)
import Hecate.IO (evalCommand, getHome)
import Hecate.IO.Parser (runCLIParser)
import Hecate.Types (AppContext (..), runAppM)
import System.Exit
import System.IO
import qualified Database.SQLite.Simple as SQLite

createContext :: IO AppContext
createContext = do
  home       <- getHome >>= maybe (error "Can't find my way HOME") pure
  connection <- SQLite.open (home ++ "/.hecate/data.db")
  _          <- initDatabase connection
  return $ AppContext (home ++ "/.hecate/auth.json") connection

{-# ANN runApp "HLint: ignore Use print" #-}
runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command  <- runCLIParser
  response <- runAppM ctx (evalCommand command)
  case response of
    Left err  -> hPrint stderr err >> return (ExitFailure 1)
    Right out -> hPrint stdout out >> return ExitSuccess

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (_conn ctx)

main :: IO ()
main = bracket createContext finalize runApp >>= exitWith
