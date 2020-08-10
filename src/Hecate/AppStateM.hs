{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.AppStateM
  ( AppStateM
  , runAppStateM
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State    (MonadState, StateT, runStateT)
import           Lens.Family2           (view)
import           Lens.Family2.State     (uses, (%=))

import           Hecate.AppState        (AppState, HasAppState (..))
import qualified Hecate.AppState        as AppState
import           Hecate.Data            (Config (..), HasConfig (..), SchemaVersion (..))
import           Hecate.Interfaces


-- * AppStateM

newtype AppStateM a = AppStateM { unAppStateM :: ReaderT Config (StateT AppState IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Config
           , MonadState AppState
           , MonadEncrypt
           , MonadInteraction
           , MonadAppError
           )

instance MonadThrow AppStateM where
  throwM = liftIO . throwM

runAppStateM :: AppStateM a -> AppState -> Config -> IO (a, AppState)
runAppStateM m state cfg = runStateT (runReaderT (unAppStateM m) cfg) state

-- * Instances

instance MonadConfigReader AppStateM where
  askConfig = asks (view config)

instance MonadStore AppStateM where
  put             e     = appState %= AppState.put    e
  delete          e     = appState %= AppState.delete e
  query           q     = uses appState (AppState.query  q)
  selectAll             = uses appState AppState.selectAll
  getCount              = uses appState AppState.getCount
  getCountOfKeyId kid   = uses appState (AppState.getCountOfKeyId kid)
  createTable           = pure ()
  migrate         _   _ = pure ()
  currentSchemaVersion  = pure (SchemaVersion 2)
