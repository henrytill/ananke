{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Except
import Control.Exception
import Hecate.Database (getSchemaVersion, initDatabase)
import Hecate.IO (evalCommand)
import Hecate.IO.Config (configure)
import Hecate.IO.Parser (runCLIParser)
import Hecate.Printing
import Hecate.Types (AppConfig(..), AppContext(..), AppError(..), runAppM)
import System.Console.ANSI (hSupportsANSI)
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen
import qualified Database.SQLite.Simple as SQLite

configToContext :: (MonadIO m, MonadError AppError m) => AppConfig -> m AppContext
configToContext AppConfig{..} = do
  connection    <- liftIO (SQLite.open (appConfigDataDirectory ++ "/db/db.sqlite"))
  schemaVersion <- getSchemaVersion (appConfigDataDirectory ++ "/db/schema")
  _             <- initDatabase connection schemaVersion
  return (AppContext appConfigKeyId connection)

hPutDocWrapper :: Handle -> Doc -> Doc -> IO ()
hPutDocWrapper h f g = do
  supportsANSI <- hSupportsANSI h
  if supportsANSI
    then hPutDoc h f
    else hPutDoc h g

initialize :: IO AppContext
initialize = do
  errOrCtx <- runExceptT (configure >>= configToContext)
  case errOrCtx of
    Left err  -> hPrint stderr err >> exitFailure
    Right ctx -> return ctx

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command  <- runCLIParser
  response <- runAppM ctx (evalCommand command)
  case response of
    Left err  ->
      hPutDoc stderr (prettyError command err) >>
      return (ExitFailure 1)
    Right out ->
      hPutDocWrapper stdout (ansiPrettyResponse command out) (prettyResponse command out) >>
      return ExitSuccess

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (_conn ctx)

main :: IO ()
main = bracket initialize finalize runApp >>= exitWith
