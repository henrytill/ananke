{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.AppStateM
  ( AppStateM
  , run
  ) where

import           Control.Monad            (when)
import           Control.Monad.Catch      (MonadThrow (..))
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State      (MonadState, StateT, gets, modify, runStateT)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.List                as List
import           Lens.Family2

import           Hecate.AppState          (AppState, appStateDirty)
import qualified Hecate.AppState          as AppState
import           Hecate.Data              (Config, configDataFile, entryKeyOrder)
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

writeState :: (MonadAppError m, MonadInteraction m) => AppState -> Config -> m ()
writeState state cfg = when (state ^. appStateDirty) $ do
  let dataFile = cfg ^. configDataFile
      entries  = AppState.selectAll state
      aesonCfg = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}
      dataBS   = AesonPretty.encodePretty' aesonCfg (List.sort entries)
  writeFileFromLazyByteString dataFile dataBS

run :: AppStateM a -> AppState -> Config -> IO a
run m state cfg = do
  (res, state') <- runAppStateM m state cfg
  writeState state' cfg
  return res

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
  createTable           = undefined
  migrate         _   _ = undefined
  currentSchemaVersion  = undefined
