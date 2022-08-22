module Hecate
  ( module Hecate.Configuration
  , run
  ) where

import qualified Control.Exception       as Exception
import           System.Exit             (ExitCode (..))
import qualified System.IO               as IO
import qualified Text.PrettyPrint.Leijen as Leijen

import qualified Hecate.Backend.JSON     as JSON
import qualified Hecate.Backend.SQLite   as SQLite
import           Hecate.Configuration    (Backend (..), Config (..), configure)
import           Hecate.Error            (AppError)
import           Hecate.Evaluator        (Command, Response (..))
import qualified Hecate.Evaluator        as Evaluator
import qualified Hecate.Parser           as Parser
import qualified Hecate.Printing         as Printing


handleError :: Command -> AppError -> IO ExitCode
handleError cmd err = Leijen.hPutDoc IO.stderr (Printing.prettyError cmd err) >> return (ExitFailure 1)

handleResponse :: Command -> Response -> IO ExitCode
handleResponse cmd res =
  let
    pr = Leijen.hPutDoc IO.stdout . Printing.prettyResponse cmd
  in case res of
    (SingleEntry     _  _) -> pr res >> return ExitSuccess
    (MultipleEntries [] _) -> pr res >> return (ExitFailure 1)
    (MultipleEntries _  _) -> pr res >> return ExitSuccess
    _                      -> return ExitSuccess

runJSONApp :: (Config, JSON.AppState) -> IO ExitCode
runJSONApp (cfg, state) = do
  cmd <- Parser.runCLIParser
  Exception.catch (JSON.run (Evaluator.eval cmd) state cfg >>= handleResponse cmd)
                  (handleError cmd)

runSQLiteApp :: SQLite.AppContext -> IO ExitCode
runSQLiteApp ctx = do
  cmd <- Parser.runCLIParser
  Exception.catch (SQLite.run (Evaluator.setup >> Evaluator.eval cmd) ctx >>= handleResponse cmd)
                  (handleError cmd)

run :: Config -> IO ExitCode
run cfg = case configBackend cfg of
  JSON   -> Exception.bracket (JSON.initialize   cfg) JSON.finalize   runJSONApp
  SQLite -> Exception.bracket (SQLite.initialize cfg) SQLite.finalize runSQLiteApp
