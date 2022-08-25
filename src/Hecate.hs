{-# LANGUAGE CPP #-}

module Hecate
  ( module Hecate.Configuration
  , run
  ) where

import qualified Control.Exception     as Exception
import           System.Exit           (ExitCode (..))
import qualified System.IO             as IO
import           Text.PrettyPrint      (Doc, Mode (..), Style (..), empty, renderStyle, style)

#ifdef BACKEND_JSON
import qualified Hecate.Backend.JSON   as JSON
import           Hecate.Error          (AppError)
#else
import           Hecate.Error          (AppError (Configuration))
#endif

import qualified Hecate.Backend.SQLite as SQLite
import           Hecate.Configuration  (Backend (..), Config (..), configure)
import           Hecate.Evaluator      (Response (..))
import qualified Hecate.Evaluator      as Evaluator
import qualified Hecate.Parser         as Parser
import qualified Hecate.Printing       as Printing


render :: Doc -> String
render = renderStyle style{mode = LeftMode}

handleError :: AppError -> IO ExitCode
handleError err =
  let
    pr = IO.hPutStrLn IO.stderr . render . Printing.prettyError
  in
    pr err >> return (ExitFailure 1)

handleResponse :: Response -> IO ExitCode
handleResponse res =
  let
    pr r = case Printing.prettyResponse r of
      doc | doc == empty -> return ()
          | otherwise    -> IO.hPutStrLn IO.stdout . render $ doc
  in case res of
    (MultipleEntries [] _) -> pr res >> return (ExitFailure 1)
    _                      -> pr res >> return ExitSuccess

runSQLiteApp :: SQLite.AppContext -> IO ExitCode
runSQLiteApp ctx = do
  cmd <- Parser.runCLIParser
  Exception.catch (SQLite.run (Evaluator.setup >> Evaluator.eval cmd) ctx >>= handleResponse)
                  handleError

#ifdef BACKEND_JSON
runJSONApp :: (Config, JSON.AppState) -> IO ExitCode
runJSONApp (cfg, state) = do
  cmd <- Parser.runCLIParser
  Exception.catch (JSON.run (Evaluator.eval cmd) state cfg >>= handleResponse)
                  handleError
#endif

run :: Config -> IO ExitCode
run cfg = case configBackend cfg of
  SQLite -> Exception.bracket (SQLite.initialize cfg) SQLite.finalize runSQLiteApp
#ifdef BACKEND_JSON
  JSON   -> Exception.bracket (JSON.initialize   cfg) JSON.finalize   runJSONApp
#else
  JSON   -> Exception.throwIO $ Configuration "JSON backend not available.  Please rebuild with backend-json flag enabled."
#endif
