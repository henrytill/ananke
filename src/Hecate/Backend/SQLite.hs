{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS -Wno-unused-top-binds #-}

module Hecate.Backend.SQLite
  ( SQLite
  , run
  , initialize
  , finalize
  , AppContext (..)
  ) where

import           Control.Monad.Catch              (MonadThrow (..))
import qualified Control.Monad.Except             as Except
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Data.Text                        as T
import qualified Database.SQLite3                 as SQLite3

import           Hecate.Backend.SQLite.AppContext (AppContext (..))
import           Hecate.Data                      (Config, configDatabaseDirectory, configDatabaseFile)
import           Hecate.Interfaces


-- * SQLite

newtype SQLite a = SQLite { unSQLite :: ReaderT AppContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadEncrypt
           , MonadInteraction
           , MonadAppError
           )

instance MonadThrow SQLite where
  throwM = liftIO . throwM

runSQLite :: SQLite a -> AppContext -> IO a
runSQLite m = runReaderT (unSQLite m)

run :: SQLite a -> AppContext -> IO a
run = runSQLite

initialize :: Config -> IO AppContext
initialize cfg = do
  let dbDir  = configDatabaseDirectory cfg
      dbFile = configDatabaseFile cfg
  dbDirExists <- doesDirectoryExist dbDir
  Except.unless dbDirExists (createDirectory dbDir)
  AppContext cfg <$> SQLite3.open (T.pack dbFile)

finalize :: AppContext -> IO ()
finalize ctx = SQLite3.close db
  where
    db = appContextDatabase ctx

-- * Instances

instance MonadConfigReader SQLite where
  askConfig = asks appContextConfig

withDatabase :: (SQLite3.Database -> SQLite a) -> SQLite a
withDatabase f = ask >>= f . appContextDatabase

instance MonadStore SQLite where
  put                  = undefined
  delete               = undefined
  query                = undefined
  selectAll            = undefined
  getCount             = undefined
  getCountOfKeyId      = undefined
  createTable          = undefined
  migrate              = undefined
  currentSchemaVersion = undefined
