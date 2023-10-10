{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ananke.Backend.SQLite
  ( SQLite
  , run
  , initialize
  , finalize
  , AppContext (..)
  ) where

import           Control.Monad                    (unless)
import           Control.Monad.Catch              (MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Data.Text                        as T
import qualified Database.SQLite3                 as SQLite3

import           Ananke.Backend.SQLite.AppContext (AppContext (..))
import qualified Ananke.Backend.SQLite.Database   as Database
import           Ananke.Data                      (Config, configDatabaseDirectory, configDatabaseFile)
import           Ananke.Interfaces


-- * SQLite

newtype SQLite a = MkSQLite { unSQLite :: ReaderT AppContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadAppError
           , MonadEncrypt
           , MonadInteraction
           )

runSQLite :: SQLite a -> AppContext -> IO a
runSQLite = runReaderT . unSQLite

run :: SQLite a -> AppContext -> IO a
run = runSQLite

initialize :: Config -> IO AppContext
initialize cfg = do
  let dbDir  = configDatabaseDirectory cfg
      dbFile = configDatabaseFile cfg
  dbDirExists <- doesDirectoryExist dbDir
  unless dbDirExists (createDirectory dbDir)
  MkAppContext cfg <$> SQLite3.open (T.pack dbFile)

finalize :: AppContext -> IO ()
finalize = SQLite3.close . appContextDatabase

-- * Instances

instance MonadThrow SQLite where
  throwM = liftIO . throwM

instance MonadConfigReader SQLite where
  askConfig = asks appContextConfig

withDatabase :: (SQLite3.Database -> SQLite a) -> SQLite a
withDatabase f = ask >>= f . appContextDatabase

instance MonadStore SQLite where
  put             e       = withDatabase $ \db -> Database.put             db e
  delete          e       = withDatabase $ \db -> Database.delete          db e
  runQuery        q       = withDatabase $ \db -> Database.runQuery        db q
  selectAll               = withDatabase $ \db -> Database.selectAll       db
  getCount                = withDatabase $ \db -> Database.getCount        db
  getCountOfKeyId kid     = withDatabase $ \db -> Database.getCountOfKeyId db kid
  createTable             = withDatabase $ \db -> Database.createTable     db
  migrate         sv  kid = withDatabase $ \db -> Database.migrate         db sv  kid
