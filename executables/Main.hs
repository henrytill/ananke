{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import System.Console.ANSI (hSupportsANSI)
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen
import qualified Database.SQLite.Simple as SQLite

import Hecate.Context
import Hecate.Database
import Hecate.Error
import Hecate.Evaluator
import Hecate.Parser
import Hecate.Printing


hPutDocWrapper :: Handle -> Doc -> Doc -> IO ()
hPutDocWrapper h f g = do
  supportsANSI <- hSupportsANSI h
  if supportsANSI
    then hPutDoc h f
    else hPutDoc h g

initialize :: IO AppContext
initialize = runExceptT (getDataDir >>= configure >>= createContext) >>= processResult
  where
    processResult (Left err)  = hPrint stderr err >> exitFailure
    processResult (Right ctx) = return ctx

runM :: AppContext -> ReaderT AppContext (ExceptT AppError IO) a -> IO (Either AppError a)
runM ctx = runExceptT . flip runReaderT ctx

runApp :: AppContext -> IO ExitCode
runApp ctx = do
  command  <- runCLIParser
  response <- runM ctx (eval command)
  case response of
    Left err  ->
      hPutDoc stderr (prettyError command err) >>
      return (ExitFailure 1)
    Right out ->
      hPutDocWrapper stdout (ansiPrettyResponse command out) (prettyResponse command out) >>
      return ExitSuccess

finalize :: AppContext -> IO ()
finalize ctx = SQLite.close (appContextConnection ctx)

main :: IO ()
main = bracket initialize finalize runApp >>= exitWith
