{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Ananke.Backend.JSON
  ( JSON
  , encodeJSON
  , run
  , initialize
  , finalize
  , AppState
  ) where

import           Control.Monad                (unless, when)
import           Control.Monad.Catch          (MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State          (MonadState, StateT, gets, modify, runStateT)
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as AesonPretty
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.List                    as List

import           Ananke.Backend.JSON.AppState (AppState, appStateDirty)
import qualified Ananke.Backend.JSON.AppState as AppState
import           Ananke.Data                  (Config, Entry, configDataDirectory, configDataFile, entryKeyOrder)
import           Ananke.Interfaces


-- * JSON

newtype JSON a = MkJSON { unJSON :: ReaderT Config (StateT AppState IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Config
           , MonadState AppState
           , MonadAppError
           , MonadEncrypt
           , MonadFilesystem
           , MonadInteraction
           )

runJSON :: JSON a -> AppState -> Config -> IO (a, AppState)
runJSON m state cfg = runStateT (runReaderT (unJSON m) cfg) state

encodeJSON :: [Entry] -> BSL.ByteString
encodeJSON = appendNewline . AesonPretty.encodePretty' config . List.sort
  where
    appendNewline = flip BSL.append "\n"
    config = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}

writeState :: (MonadAppError m, MonadFilesystem m) => AppState -> Config -> m ()
writeState state cfg = when (appStateDirty state) $ writeFileFromLazyByteString jsonFile output
  where
    jsonFile = configDataFile cfg
    output = encodeJSON (AppState.selectAll state)

run :: JSON a -> AppState -> Config -> IO a
run m state cfg = do
  (res, state') <- runJSON m state cfg
  writeState state' cfg
  return res

createState :: (MonadAppError m, MonadFilesystem m) => Config -> m AppState
createState cfg = do
  let dataDir  = configDataDirectory cfg
      dataFile = configDataFile cfg
  dataDirExists <- doesDirectoryExist dataDir
  unless dataDirExists $ createDirectory dataDir
  input <- readFileAsLazyByteString dataFile
  maybe (databaseError "unable to decode data.json") (return . AppState.mkAppState) (Aeson.decode input)

initialize :: (MonadAppError m, MonadFilesystem m) => Config -> m (Config, AppState)
initialize cfg = do
  state <- createState cfg
  return (cfg, state)

finalize :: (MonadAppError m, MonadInteraction m) => (Config, AppState) -> m ()
finalize _ = return ()

-- * Instances

instance MonadThrow JSON where
  throwM = liftIO . throwM

instance MonadConfigReader JSON where
  askConfig = ask

instance MonadStore JSON where
  put             e = modify $ AppState.put             e
  delete          e = modify $ AppState.delete          e
  runQuery        q = gets   $ AppState.runQuery        q
  selectAll         = gets     AppState.selectAll
  getCount          = gets     AppState.getCount
  getCountOfKeyId k = gets   $ AppState.getCountOfKeyId k
