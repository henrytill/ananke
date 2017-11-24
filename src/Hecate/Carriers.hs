{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Carriers
  ( SQLiteStoreT
  , runSQLiteStoreT
  , EncryptT
  , runEncryptT
  , InteractionT
  , runInteractionT
  , AppM
  , runAppM
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader      (MonadReader (..), ReaderT, runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (..))
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text.IO              as TIO
import           Data.Time.Clock           (getCurrentTime)
import qualified Database.SQLite.Simple    as SQLite
import           Lens.Family2
import qualified System.Directory          as Directory
import           System.IO                 (hFlush, stdout)
import qualified System.Posix.Env          as Env

import           Hecate.Context            (HasAppContext (..), AppContext)
import qualified Hecate.Database           as DB
import qualified Hecate.GPG                as GPG
import           Hecate.Interfaces

-- * SQLiteStoreT

newtype SQLiteStoreT r m a = SQLiteStoreT { runSQLiteStoreT :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader r
           )

instance MonadTrans (SQLiteStoreT r) where
  lift = SQLiteStoreT

withConnection
  :: (MonadReader r m, HasAppContext r)
  => (SQLite.Connection -> m a)
  -> m a
withConnection f = ask >>= \ ctx -> f (ctx ^. appContextConnection)

instance (MonadThrow m, MonadIO m, MonadReader r m, HasAppContext r) =>
         MonadStore (SQLiteStoreT r m) where
  put             e       = withConnection (`DB.put` e)
  delete          e       = withConnection (`DB.delete` e)
  query           q       = withConnection (`DB.query` q)
  selectAll               = withConnection DB.selectAll
  getCount                = withConnection DB.getCount
  getCountOfKeyId kid     = withConnection (`DB.getCountOfKeyId` kid)
  createTable             = withConnection DB.createTable
  migrate         sv  kid = withConnection (\ conn -> DB.migrate conn sv kid)

instance MonadThrow m => MonadThrow (SQLiteStoreT r m) where
  throwM = lift . throwM


-- * EncryptM

newtype EncryptT m a = EncryptT { runEncryptT :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader r
           )

instance MonadTrans EncryptT where
  lift = EncryptT

instance MonadStore m => MonadStore (EncryptT m) where
  put             = lift . put
  delete          = lift . delete
  query           = lift . query
  selectAll       = lift selectAll
  getCount        = lift getCount
  getCountOfKeyId = lift . getCountOfKeyId
  createTable     = lift createTable
  migrate sv kid  = lift (migrate sv kid)

instance (MonadThrow m, MonadIO m) => MonadEncrypt (EncryptT m) where
  encrypt = GPG.encrypt
  decrypt = GPG.decrypt

instance MonadThrow m => MonadThrow (EncryptT m) where
  throwM = lift . throwM


-- * InteractionT

newtype InteractionT m a = InteractionT { runInteractionT :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader r
           )

instance MonadTrans InteractionT where
  lift = InteractionT

instance MonadStore m => MonadStore (InteractionT m) where
  put             = lift . put
  delete          = lift . delete
  query           = lift . query
  selectAll       = lift selectAll
  getCount        = lift getCount
  getCountOfKeyId = lift . getCountOfKeyId
  createTable     = lift createTable
  migrate sv kid  = lift (migrate sv kid)

instance MonadEncrypt m => MonadEncrypt (InteractionT m) where
  encrypt kid pt = lift (encrypt kid pt)
  decrypt        = lift . decrypt

instance (MonadThrow m, MonadIO m) => MonadInteraction (InteractionT m)  where
  now                              = liftIO getCurrentTime
  doesFileExist                    = liftIO . Directory.doesFileExist
  doesDirectoryExist               = liftIO . Directory.doesDirectoryExist
  createDirectory                  = liftIO . Directory.createDirectory
  openSQLiteFile                   = liftIO . SQLite.open
  closeSQLiteConnection            = liftIO . SQLite.close
  readFileAsString                 = liftIO . readFile
  readFileAsText                   = liftIO . TIO.readFile
  readFileAsLazyByteString         = liftIO . BSL.readFile
  writeFileFromString fp s         = liftIO (writeFile fp s)
  writeFileFromLazyByteString fp s = liftIO (BSL.writeFile fp s)
  getEnv                           = liftIO . Env.getEnv
  message                          = liftIO . putStrLn
  prompt s                         = liftIO (putStr s >> hFlush stdout >> getLine)

instance MonadThrow m => MonadThrow (InteractionT m) where
  throwM = lift . throwM


-- * AppM

newtype AppM a
  = AppM { unAppM :: InteractionT (EncryptT (SQLiteStoreT AppContext (ReaderT AppContext IO))) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadStore
           , MonadEncrypt
           , MonadInteraction
           )

runAppM :: AppM a -> AppContext -> IO a
runAppM m = runReaderT (runSQLiteStoreT (runEncryptT (runInteractionT (unAppM m))))

instance MonadThrow AppM where
  throwM = liftIO . throwM
