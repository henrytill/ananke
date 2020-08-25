{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Backend.SQLite
  ( SQLite
  , runSQLite
  , createContext
  , initialize
  , finalize
  , AppContext
  , HasAppContext (..)
  ) where

import           Control.Monad.Catch              (MonadThrow (..))
import qualified Control.Monad.Except             as Except
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Database.SQLite.Simple           as SQLite
import           Lens.Family2                     (view, (^.))

import           Hecate.Backend.SQLite.AppContext (AppContext (..), HasAppContext (..))
import qualified Hecate.Backend.SQLite.Database   as Database
import           Hecate.Configuration             (configure)
import           Hecate.Data                      (Config, HasConfig (..))
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

createContext :: MonadInteraction m => Config -> m AppContext
createContext cfg = do
  let dbDir  = cfg ^. configDatabaseDirectory
      dbFile = cfg ^. configDatabaseFile
  dbDirExists <- doesDirectoryExist dbDir
  Except.unless dbDirExists (createDirectory dbDir)
  AppContext cfg <$> openSQLiteFile dbFile

initialize :: (MonadAppError m, MonadInteraction m) => m AppContext
initialize = configure >>= createContext

finalize :: MonadInteraction m => AppContext -> m ()
finalize ctx = closeSQLiteConnection conn
  where
    conn = ctx ^. appContextConnection

-- * Instances

instance MonadConfigReader SQLite where
  askConfig = asks (view appContextConfig)

withConnection :: (SQLite.Connection -> SQLite a) -> SQLite a
withConnection f = ask >>= f . view appContextConnection

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
