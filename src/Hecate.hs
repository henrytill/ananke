{-# LANGUAGE CPP #-}

module Hecate
  ( module Hecate.Configuration
  , run
  ) where

import qualified Control.Exception     as Exception
import           System.Exit           (ExitCode (..))
import qualified System.IO             as IO

#ifdef BACKEND_JSON
import qualified Hecate.Backend.JSON   as JSON
#endif

import qualified Hecate.Backend.SQLite as SQLite
import           Hecate.Configuration  (Backend (..), Config (..), configure)
import           Hecate.Error          (AppError (..))
import           Hecate.Evaluator      (Response (..))
import qualified Hecate.Evaluator      as Evaluator
import qualified Hecate.Parser         as Parser
import           Hecate.Printing       (prettyError, prettyResponse, render)


handleError :: AppError -> IO ExitCode
handleError err =  pr err >> return (ExitFailure 1)
  where
    pr = IO.hPutStrLn IO.stderr . render . prettyError

handleResponse :: Response -> IO ExitCode
handleResponse res = case res of
  (MultipleEntries [] _) -> pr res >> return (ExitFailure 1)
  _                      -> pr res >> return ExitSuccess
  where
    pr r = case prettyResponse r of
      doc | doc == mempty -> return ()
          | otherwise     -> putStrLn . render $ doc

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
