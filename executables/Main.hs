module Main where

import Control.Exception
import Control.Monad
import Hecate.Database (initDatabase)
import Hecate.IO (evalCommand, getHome)
import Hecate.IO.Parser (runCLIParser)
import Hecate.Printing
import Hecate.Types (AppContext (..), runAppM)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen
import qualified Database.SQLite.Simple as SQLite

createContext :: IO AppContext
createContext = do
  home       <- getHome >>= maybe (error "Can't find my way HOME") pure
  let dataDir = home ++ "/.hecate"
  dirExists  <- doesDirectoryExist dataDir
  unless dirExists (createDirectory dataDir)
  connection <- SQLite.open (dataDir ++ "/data.db")
  _          <- initDatabase connection
  return $ AppContext (home ++ "/.hecate/auth.json") connection

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command  <- runCLIParser
  response <- runAppM ctx (evalCommand command)
  case response of
    Left err  -> hPutDoc stderr (ppAppError command err) >> return (ExitFailure 1)
    Right out -> hPutDoc stdout (ppResponse command out) >> return ExitSuccess

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (_conn ctx)

main :: IO ()
main = bracket createContext finalize runApp >>= exitWith
