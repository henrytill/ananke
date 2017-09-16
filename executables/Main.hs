module Main (main) where

import           Control.Exception
import           Control.Monad.Reader
import qualified Database.SQLite.Simple       as SQLite
import           System.Console.ANSI          (hSupportsANSI)
import           System.Exit
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen

import           Hecate.Context
import           Hecate.Database
import           Hecate.Error
import           Hecate.Evaluator
import           Hecate.Parser
import           Hecate.Printing

hPutDocWrapper :: Handle -> Doc -> Doc -> IO ()
hPutDocWrapper h f g = do
  supportsANSI <- hSupportsANSI h
  if supportsANSI
    then hPutDoc h f
    else hPutDoc h g

initialize :: IO AppContext
initialize = getDataDir >>= configure >>= createContext

runM :: AppContext -> ReaderT AppContext IO a -> IO a
runM = flip runReaderT

exceptionHandler :: Command -> AppError -> IO ExitCode
exceptionHandler command err =
  hPutDoc stderr (prettyError command err) >>
  return (ExitFailure 1)

resultHandler :: Command -> Response -> IO ExitCode
resultHandler command res =
  hPutDocWrapper stdout (ansiPrettyResponse command res) (prettyResponse command res) >>
  return ExitSuccess

runApp :: AppContext -> IO ExitCode
runApp ctx =
  runCLIParser >>= \command ->
  catch (runM ctx (eval command) >>= resultHandler command) (exceptionHandler command)

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (_appContextConnection ctx)

main :: IO ()
main = bracket initialize finalize runApp >>= exitWith
