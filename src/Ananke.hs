{-# LANGUAGE CPP #-}

module Ananke
  ( module Ananke.Configuration
  , run
  ) where

import qualified Control.Exception     as Exception
import           System.Exit           (ExitCode (..))
import qualified System.IO             as IO

#ifdef BACKEND_JSON
import qualified Ananke.Backend.JSON   as JSON
#endif

import qualified Ananke.Backend.SQLite as SQLite
import           Ananke.Configuration  (Backend (..), Config (..), configure)
import           Ananke.Error          (AppError (..))
import           Ananke.Evaluator      (Response (..))
import qualified Ananke.Evaluator      as Evaluator
import qualified Ananke.Parser         as Parser
import           Ananke.Printing       (prettyError, prettyResponse, render)


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
  Exception.catch (SQLite.run (SQLite.setup >> Evaluator.eval cmd) ctx >>= handleResponse)
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
  JSON -> Exception.bracket (JSON.preInitialize cfg >> JSON.initialize cfg) JSON.finalize runJSONApp
#else
  JSON -> Exception.throwIO $ Configuration "JSON backend not available.  Please rebuild with backend-json flag enabled."
#endif
