{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.AppStateM
  ( AppStateM
  , runAppStateM
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (MonadState, StateT, gets, modify, runStateT)

import           Hecate.AppState        (AppState)
import qualified Hecate.AppState        as AppState
import           Hecate.Data            (Config (..), SchemaVersion (..))
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
  askConfig = ask

instance MonadStore AppStateM where
  put             e     = modify $ AppState.put    e
  delete          e     = modify $ AppState.delete e
  query           q     = gets (AppState.query  q)
  selectAll             = gets AppState.selectAll
  getCount              = gets AppState.getCount
  getCountOfKeyId kid   = gets (AppState.getCountOfKeyId kid)
  createTable           = pure ()
  migrate         _   _ = pure ()
  currentSchemaVersion  = pure (SchemaVersion 2)
