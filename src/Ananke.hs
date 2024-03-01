module Ananke
  ( module Ananke.Configuration,
    module Ananke.Parser,
    run,
  )
where

import Ananke.Backend (currentSchemaVersion)
import Ananke.Backend.JSON qualified as JSON
import Ananke.Backend.SQLite qualified as SQLite
import Ananke.Configuration (Backend (..), Config (..), configure)
import Ananke.Error (AppError (..))
import Ananke.Evaluator (Command, Response (..))
import Ananke.Evaluator qualified as Evaluator
import Ananke.Parser (parseCommand)
import Ananke.Printing (prettyError, prettyResponse, render)
import Control.Exception qualified as Exception
import System.Exit (ExitCode (..))
import System.IO qualified as IO

handleError :: AppError -> IO ExitCode
handleError err = pr err >> return (ExitFailure 1)
  where
    pr = IO.hPutStrLn IO.stderr . render . prettyError

handleResponse :: Response -> IO ExitCode
handleResponse res = case res of
  (MultipleEntries [] _) -> pr res >> return (ExitFailure 1)
  _ -> pr res >> return ExitSuccess
  where
    pr r = case prettyResponse r of
      doc
        | doc == mempty -> return ()
        | otherwise -> putStrLn . render $ doc

runSQLiteApp :: Command -> SQLite.AppContext -> IO ExitCode
runSQLiteApp cmd ctx =
  Exception.catch
    (SQLite.run (SQLite.setup currentSchemaVersion >> Evaluator.eval cmd) ctx >>= handleResponse)
    handleError

runJSONApp :: Command -> (Config, JSON.AppState) -> IO ExitCode
runJSONApp cmd (cfg, state) =
  Exception.catch
    (JSON.run (Evaluator.eval cmd) state cfg >>= handleResponse)
    handleError

run :: Command -> Config -> IO ExitCode
run cmd cfg = case configBackend cfg of
  SQLite ->
    Exception.bracket
      (SQLite.initialize cfg)
      SQLite.finalize
      (runSQLiteApp cmd)
  JSON ->
    Exception.bracket
      (JSON.preInitialize cfg currentSchemaVersion >> JSON.initialize cfg)
      JSON.finalize
      (runJSONApp cmd)
