{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Hecate.Carriers
  ( AppM
  , runAppM
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader         (MonadReader (..), ReaderT, runReaderT)
import           Control.Monad.Trans.Class    (MonadTrans (..))
import           Data.Char                    (toLower)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import qualified Database.SQLite.Simple       as SQLite
import           Lens.Family2
import           System.IO                    (hFlush, stdout)

import           Hecate.Context               (HasAppContext (..), AppContext)
import qualified Hecate.Database              as DB
import           Hecate.Error
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

instance (MonadIO m, MonadReader r m, HasAppContext r) =>
         MonadStore (SQLiteStoreT r m) where
  put             e   = withConnection (`DB.put` e)
  delete          e   = withConnection (`DB.delete` e)
  query           q   = withConnection (`DB.query` q)
  selectAll           = withConnection DB.selectAll
  getCount            = withConnection DB.getCount
  getCountOfKeyId kid = withConnection (`DB.getCountOfKeyId` kid)
  reencryptAll    kid = withConnection (`DB.reencryptAll` kid)


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
  reencryptAll    = lift . reencryptAll

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

instance MonadIO m => MonadInteraction (InteractionT m)  where
  now :: InteractionT m UTCTime
  now = liftIO getCurrentTime

  prompt :: String -> InteractionT m String
  prompt s = liftIO (flushStr s >> getLine)

  binaryChoice :: String -> InteractionT m a -> InteractionT m a -> InteractionT m a
  binaryChoice s yes no = do
    ans <- liftIO (flushStr (s ++ " [N/y] ") >> getLine)
    case map toLower ans of
      ""   -> no
      "n"  -> no
      "y"  -> yes
      _    -> liftIO (throwIO (Default "Please answer y or n"))


-- * AppM

newtype AppM a
  = AppM { unAppM :: InteractionT (SQLiteStoreT AppContext (ReaderT AppContext IO)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadStore
           , MonadInteraction
           )

runAppM :: AppM a -> AppContext -> IO a
runAppM m = runReaderT (runSQLiteStoreT (runInteractionT (unAppM m)))
