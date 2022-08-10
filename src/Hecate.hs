module Hecate
  ( module Hecate.Configuration
  , run
  ) where

import qualified Control.Exception           as Exception
import           System.Exit                 (ExitCode (..))
import qualified System.IO                   as IO
import qualified Text.PrettyPrint.Leijen     as Leijen

import qualified Hecate.Backend.JSON         as JSON
import qualified Hecate.Backend.SQLiteSimple as SQLiteSimple
import           Hecate.Configuration        (Backend (..), Config (..), configure)
import           Hecate.Error                (AppError)
import           Hecate.Evaluator            (Command, Response (..))
import qualified Hecate.Evaluator            as Evaluator
import qualified Hecate.Parser               as Parser
import qualified Hecate.Printing             as Printing


exceptionHandler :: Command -> AppError -> IO ExitCode
exceptionHandler command err = do
  Leijen.hPutDoc IO.stderr (Printing.prettyError command err)
  return (ExitFailure 1)

resultHandler :: Command -> Response -> IO ExitCode
resultHandler command res =
  let
    pr r = Leijen.hPutDoc IO.stdout (Printing.prettyResponse command r)
  in case res of
    (SingleEntry     _  _) -> pr res >> return ExitSuccess
    (MultipleEntries [] _) -> pr res >> return (ExitFailure 1)
    (MultipleEntries _  _) -> pr res >> return ExitSuccess
    _                      -> return ExitSuccess

runJSONApp :: (Config, JSON.AppState) -> IO ExitCode
runJSONApp (cfg, state) = do
  command <- Parser.runCLIParser
  Exception.catch (JSON.run (Evaluator.eval command) state cfg >>= resultHandler command)
                  (exceptionHandler command)

runSQLiteSimpleApp :: SQLiteSimple.AppContext -> IO ExitCode
runSQLiteSimpleApp ctx = do
  command <- Parser.runCLIParser
  Exception.catch (SQLiteSimple.run (Evaluator.setup >> Evaluator.eval command) ctx >>= resultHandler command)
                  (exceptionHandler command)

run :: Config -> IO ExitCode
run cfg = case configBackend cfg of
  JSON         -> Exception.bracket (JSON.initialize         cfg) JSON.finalize         runJSONApp
  SQLiteSimple -> Exception.bracket (SQLiteSimple.initialize cfg) SQLiteSimple.finalize runSQLiteSimpleApp
  SQLite       -> undefined
