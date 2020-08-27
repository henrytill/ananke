{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Database.SQLite.Simple           as SQLite

import           Hecate.Backend.SQLite.AppContext (AppContext (..))
import qualified Hecate.Backend.SQLite.Database   as Database
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

initialize :: MonadInteraction m => Config -> m AppContext
initialize cfg = do
  let dbDir  = configDatabaseDirectory cfg
      dbFile = configDatabaseFile cfg
  dbDirExists <- doesDirectoryExist dbDir
  Except.unless dbDirExists (createDirectory dbDir)
  AppContext cfg <$> openSQLiteFile dbFile

finalize :: MonadInteraction m => AppContext -> m ()
finalize ctx = closeSQLiteConnection conn
  where
    conn = appContextConnection ctx

-- * Instances

instance MonadConfigReader SQLite where
  askConfig = asks appContextConfig

withConnection :: (SQLite.Connection -> SQLite a) -> SQLite a
withConnection f = ask >>= f . appContextConnection

instance MonadStore SQLite where
  put             e       = withConnection (\ conn -> Database.put             conn e)
  delete          e       = withConnection (\ conn -> Database.delete          conn e)
  query           q       = withConnection (\ conn -> Database.query           conn q)
  selectAll               = withConnection (\ conn -> Database.selectAll       conn)
  getCount                = withConnection (\ conn -> Database.getCount        conn)
  getCountOfKeyId kid     = withConnection (\ conn -> Database.getCountOfKeyId conn kid)
  createTable             = withConnection (\ conn -> Database.createTable     conn)
  migrate         sv  kid = withConnection (\ conn -> Database.migrate         conn sv kid)
  currentSchemaVersion    = pure Database.currentSchemaVersion
