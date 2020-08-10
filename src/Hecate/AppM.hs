{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.AppM
  ( AppM
  , runAppM
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks, runReaderT)
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2           (view)

import           Hecate.Data            (AppContext)
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
