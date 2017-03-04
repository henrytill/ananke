{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Hecate.Database (initDatabase)
import Hecate.IO (evalCommand, getHome)
import Hecate.IO.Config (getAppConfig)
import Hecate.IO.Parser (runCLIParser)
import Hecate.Printing
import Hecate.Types (AppConfig(..), AppContext(..), runAppM)
import System.Console.ANSI (hSupportsANSI)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen
import qualified Database.SQLite.Simple as SQLite

createContext :: IO AppContext
createContext = do
  home          <- getHome >>= maybe (error "Can't find my way HOME") pure
  let dataDir    = home ++ "/.hecate"
      configFile = dataDir ++ "/hecate.toml"
  dirExists     <- doesDirectoryExist dataDir
  unless dirExists (createDirectory dataDir)
  connection    <- SQLite.open (dataDir ++ "/data.db")
  _             <- initDatabase connection
  AppConfig{..} <- getAppConfig configFile
  return $ AppContext appConfigFingerprint connection

hPutDocWrapper :: Handle -> Doc -> Doc -> IO ()
hPutDocWrapper h f g = do
  supportsANSI <- hSupportsANSI h
  if supportsANSI
    then hPutDoc h f
    else hPutDoc h g

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command  <- runCLIParser
  response <- runAppM ctx (evalCommand command)
  case response of
    Left err  ->
      hPutDocWrapper stderr (ansiPrettyError command err) (prettyError command err) >>
      return (ExitFailure 1)
    Right out ->
      hPutDocWrapper stdout (ansiPrettyResponse command out) (prettyResponse command out) >>
      return ExitSuccess

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (_conn ctx)

main :: IO ()
main = bracket createContext finalize runApp >>= exitWith
