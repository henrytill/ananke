module Main (main) where

import           Control.Exception
import           System.Console.ANSI          (hSupportsANSI)
import           System.Exit
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen

import           Hecate.Carriers
import           Hecate.Context
import           Hecate.Data                  (AppContext)
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
initialize = configure >>= createContext

exceptionHandler :: Command -> AppError -> IO ExitCode
exceptionHandler command err = do
  hPutDoc stderr (prettyError command err)
  return (ExitFailure 1)

resultHandler :: Command -> Response -> IO ExitCode
resultHandler command res = do
  hPutDocWrapper stdout (ansiPrettyResponse command res) (prettyResponse command res)
  return ExitSuccess

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command <- runCLIParser
  catch (runAppM (setup >> eval command) ctx >>= resultHandler command)
        (exceptionHandler command)

main :: IO ()
main = bracket initialize finalize runApp >>= exitWith
