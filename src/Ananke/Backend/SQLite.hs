{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ananke.Backend.SQLite
  ( SQLite
  , setup
  , run
  , initialize
  , finalize
  , AppContext (..)
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQLite3

import Ananke.Backend
import Ananke.Backend.SQLite.AppContext (AppContext (..))
import qualified Ananke.Backend.SQLite.Database as Database
import Ananke.Class
import Ananke.Data (Config (..), SchemaVersion, configDatabaseDir, configDatabaseFile, configSchemaFile)


-- * SQLite

newtype SQLite a = MkSQLite { unSQLite :: ReaderT AppContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadAppError
           , MonadEncrypt
           , MonadFilesystem
           , MonadInteraction
           , MonadTime
           )

runSQLite :: SQLite a -> AppContext -> IO a
runSQLite = runReaderT . unSQLite

run :: SQLite a -> AppContext -> IO a
run = runSQLite

initialize :: Config -> IO AppContext
initialize cfg = do
  let dbDir = configDatabaseDir cfg
      dbFile = configDatabaseFile cfg
  dbDirExists <- doesDirExist dbDir
  unless dbDirExists (createDir dbDir)
  MkAppContext cfg <$> SQLite3.open (T.pack dbFile)

finalize :: AppContext -> IO ()
finalize = SQLite3.close . appContextDatabase

-- * Setup

setup :: SchemaVersion -> SQLite ()
setup current = do
  cfg <- askConfig
  let schemaFile = configSchemaFile cfg
  previous <- getSchemaVersion schemaFile
  if previous == current
    then withDatabase Database.createTable
    else do message $ mkMigrateMessage previous current
            let keyId = configKeyId cfg
            withDatabase $ \db -> Database.migrate db previous keyId
            _ <- createSchemaFile schemaFile current
            return ()

-- * Instances

instance MonadConfigReader SQLite where
  askConfig = asks appContextConfig

withDatabase :: (SQLite3.Database -> IO a) -> SQLite a
withDatabase f = ask >>= liftIO . Database.rethrow . f . appContextDatabase

instance MonadStore SQLite where
  put e = withDatabase $ \db -> Database.put db e
  delete e = withDatabase $ \db -> Database.delete db e
  runQuery q = withDatabase $ \db -> Database.runQuery db q
  selectAll = withDatabase $ \db -> Database.selectAll db
  getCount = withDatabase $ \db -> Database.getCount db
  getCountOfKeyId k = withDatabase $ \db -> Database.getCountOfKeyId db k
