{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Carriers
  ( AppM
  , runAppM
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import           Hecate.Data            (AppContext)
import           Hecate.Interfaces


-- * AppM

newtype AppM a = AppM { unAppM :: ReaderT AppContext IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadConfigReader
           , MonadStore
           , MonadEncrypt
           , MonadInteraction
           , MonadAppError
           )

instance MonadThrow AppM where
  throwM = liftIO . throwM

runAppM :: AppM a -> AppContext -> IO a
runAppM m = runReaderT (unAppM m)
