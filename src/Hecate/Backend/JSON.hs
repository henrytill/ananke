{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Backend.JSON
  ( JSON
  , run
  , initialize
  , finalize
  , AppState
  ) where

import           Control.Monad                (when)
import           Control.Monad.Catch          (MonadThrow (..))
import qualified Control.Monad.Except         as Except
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State          (MonadState, StateT, gets, modify, runStateT)
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as AesonPretty
import qualified Data.List                    as List

import           Hecate.Backend.JSON.AppState (AppState, appStateDirty)
import qualified Hecate.Backend.JSON.AppState as AppState
import           Hecate.Data                  (Config, configDataDirectory, configDataFile, entryKeyOrder)
import           Hecate.Interfaces


-- * JSON

newtype JSON a = JSON { unJSON :: ReaderT Config (StateT AppState IO) a }
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

instance MonadThrow JSON where
  throwM = liftIO . throwM

runJSON :: JSON a -> AppState -> Config -> IO (a, AppState)
runJSON m state cfg = runStateT (runReaderT (unJSON m) cfg) state

writeState :: (MonadAppError m, MonadInteraction m) => AppState -> Config -> m ()
writeState state cfg = when (appStateDirty state) $ do
  let dataFile = configDataFile cfg
      entries  = AppState.selectAll state
      aesonCfg = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}
      dataBS   = AesonPretty.encodePretty' aesonCfg (List.sort entries)
  writeFileFromLazyByteString dataFile dataBS

run :: JSON a -> AppState -> Config -> IO a
run m state cfg = do
  (res, state') <- runJSON m state cfg
  writeState state' cfg
  return res

createState :: (MonadAppError m, MonadInteraction m) => Config -> m AppState
createState cfg = do
  let dataDir  = configDataDirectory cfg
      dataFile = configDataFile cfg
  dataDirExists <- doesDirectoryExist dataDir
  Except.unless dataDirExists (createDirectory dataDir)
  dataBS <- readFileAsLazyByteString dataFile
  maybe (aesonError "could not decode") (pure . AppState.mkAppState) (Aeson.decode dataBS)

initialize :: (MonadAppError m, MonadInteraction m) => Config -> m (Config, AppState)
initialize cfg = do
  state <- createState cfg
  return (cfg, state)

finalize :: (MonadAppError m, MonadInteraction m) => (Config, AppState) -> m ()
finalize _ = pure ()

-- * Instances

instance MonadConfigReader JSON where
  askConfig = ask

instance MonadStore JSON where
  put             e     = modify $ AppState.put    e
  delete          e     = modify $ AppState.delete e
  query           q     = gets (AppState.query  q)
  selectAll             = gets AppState.selectAll
  getCount              = gets AppState.getCount
  getCountOfKeyId kid   = gets (AppState.getCountOfKeyId kid)
  createTable           = undefined
  migrate         _   _ = undefined
  currentSchemaVersion  = undefined
