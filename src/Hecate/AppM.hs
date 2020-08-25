{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.AppM
  ( AppM
  , runAppM
  , createContext
  , initialize
  , finalize
  , AppContext
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import qualified Control.Monad.Except   as Except
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2           (view, (^.))

import           Hecate.Configuration   (configure)
import           Hecate.Data            (AppContext (..), Config, HasAppContext (..), HasConfig (..))
import qualified Hecate.Database        as DB
import           Hecate.Interfaces


-- * AppM

newtype AppM a = AppM { unAppM :: ReaderT AppContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadEncrypt
           , MonadInteraction
           , MonadAppError
           )

instance MonadThrow AppM where
  throwM = liftIO . throwM

runAppM :: AppM a -> AppContext -> IO a
runAppM m = runReaderT (unAppM m)

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

instance MonadConfigReader AppM where
  askConfig = asks (view appContextConfig)

withConnection :: (SQLite.Connection -> AppM a) -> AppM a
withConnection f = ask >>= f . view appContextConnection

instance MonadStore AppM where
  put             e       = withConnection (\ conn -> DB.put             conn e)
  delete          e       = withConnection (\ conn -> DB.delete          conn e)
  query           q       = withConnection (\ conn -> DB.query           conn q)
  selectAll               = withConnection (\ conn -> DB.selectAll       conn)
  getCount                = withConnection (\ conn -> DB.getCount        conn)
  getCountOfKeyId kid     = withConnection (\ conn -> DB.getCountOfKeyId conn kid)
  createTable             = withConnection (\ conn -> DB.createTable     conn)
  migrate         sv  kid = withConnection (\ conn -> DB.migrate         conn sv kid)
  currentSchemaVersion    = pure DB.currentSchemaVersion
