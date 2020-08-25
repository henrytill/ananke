module Main (main) where

import qualified Control.Exception            as Exception
import qualified System.Console.ANSI          as ANSI
import           System.Exit                  (ExitCode (..))
import qualified System.Exit                  as Exit
import           System.IO                    (Handle)
import qualified System.IO                    as IO
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen

import           Hecate.Backend.JSON          (AppState)
import qualified Hecate.Backend.JSON          as JSON
import           Hecate.Backend.SQLite        (AppContext)
import qualified Hecate.Backend.SQLite        as SQLite
import           Hecate.Configuration         (Backend (..), Config, configure, _configBackend)
import           Hecate.Error                 (AppError)
import           Hecate.Evaluator             (Command, Response)
import qualified Hecate.Evaluator             as Evaluator
import qualified Hecate.Parser                as Parser
import qualified Hecate.Printing              as Printing


hPutDocWrapper :: Handle -> Doc -> Doc -> IO ()
hPutDocWrapper h f g = do
  supportsANSI <- ANSI.hSupportsANSI h
  if supportsANSI
    then Leijen.hPutDoc h f
    else Leijen.hPutDoc h g

exceptionHandler :: Command -> AppError -> IO ExitCode
exceptionHandler command err = do
  Leijen.hPutDoc IO.stderr (Printing.prettyError command err)
  return (ExitFailure 1)

resultHandler :: Command -> Response -> IO ExitCode
resultHandler command res = do
  hPutDocWrapper IO.stdout (Printing.ansiPrettyResponse command res) (Printing.prettyResponse command res)
  return ExitSuccess

runJSONApp :: (Config, AppState) -> IO ExitCode
runJSONApp (cfg, state) = do
  command <- Parser.runCLIParser
  Exception.catch (JSON.run (Evaluator.eval command) state cfg >>= resultHandler command)
                  (exceptionHandler command)

runSQLiteApp :: AppContext -> IO ExitCode
runSQLiteApp ctx = do
  command <- Parser.runCLIParser
  Exception.catch (SQLite.run (Evaluator.setup >> Evaluator.eval command) ctx >>= resultHandler command)
                  (exceptionHandler command)

main :: IO ()
main = do
  cfg    <- configure
  result <- case _configBackend cfg of
    JSON   -> Exception.bracket (JSON.initialize   cfg) JSON.finalize   runJSONApp
    SQLite -> Exception.bracket (SQLite.initialize cfg) SQLite.finalize runSQLiteApp
  Exit.exitWith result
