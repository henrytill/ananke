module Main (main) where

import qualified Control.Exception            as Exception
import qualified System.Console.ANSI          as ANSI
import           System.Exit                  (ExitCode (..))
import qualified System.Exit                  as Exit
import           System.IO                    (Handle)
import qualified System.IO                    as IO
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen

import qualified Hecate.Carriers              as Carriers
import qualified Hecate.Configuration         as Configuration
import           Hecate.Data                  (AppContext)
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

initialize :: IO AppContext
initialize = Configuration.configure >>= Configuration.createContext

exceptionHandler :: Command -> AppError -> IO ExitCode
exceptionHandler command err = do
  Leijen.hPutDoc IO.stderr (Printing.prettyError command err)
  return (ExitFailure 1)

resultHandler :: Command -> Response -> IO ExitCode
resultHandler command res = do
  hPutDocWrapper IO.stdout (Printing.ansiPrettyResponse command res) (Printing.prettyResponse command res)
  return ExitSuccess

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command <- Parser.runCLIParser
  Exception.catch (Carriers.runAppM (Evaluator.setup >> Evaluator.eval command) ctx >>= resultHandler command)
                  (exceptionHandler command)

main :: IO ()
main = Exception.bracket initialize Configuration.finalize runApp >>= Exit.exitWith
