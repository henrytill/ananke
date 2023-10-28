{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Ananke.Backend.JSON
  ( JSON,
    encodeJSON,
    run,
    preInitialize,
    initialize,
    finalize,
    AppState,
  )
where

import Ananke.Backend
import Ananke.Backend.JSON.AppState (AppState, appStateDirty)
import Ananke.Backend.JSON.AppState qualified as AppState
import Ananke.Class
import Ananke.Data (Config (..), Entry, SchemaVersion (..), configDataFile, configSchemaFile, entryKeyOrder)
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Aeson.KeyMap (Key, KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List

errUnableToDecode :: String
errUnableToDecode = "unable to decode data.json"

-- * JSON

newtype JSON a = MkJSON {unJSON :: ReaderT Config (StateT AppState IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadState AppState,
      MonadAppError,
      MonadEncrypt,
      MonadFilesystem,
      MonadInteraction,
      MonadTime
    )

runJSON :: JSON a -> AppState -> Config -> IO (a, AppState)
runJSON m state cfg = runStateT (runReaderT (unJSON m) cfg) state

aesonConfig :: AesonPretty.Config
aesonConfig = AesonPretty.defConfig {AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}

appendNewline :: BSL.ByteString -> BSL.ByteString
appendNewline = flip BSL.append "\n"

encodeJSON :: [Entry] -> BSL.ByteString
encodeJSON = appendNewline . AesonPretty.encodePretty' aesonConfig . List.sort

writeState :: AppState -> Config -> IO ()
writeState state cfg = when (appStateDirty state) $ writeFileBytes jsonFile output
  where
    jsonFile = configDataFile cfg
    output = encodeJSON (AppState.selectAll state)

run :: JSON a -> AppState -> Config -> IO a
run m state cfg = do
  (res, state') <- runJSON m state cfg
  writeState state' cfg
  return res

createState :: Config -> IO AppState
createState cfg = do
  let dataDir = configDataDir cfg
      dataFile = configDataFile cfg
  dataDirExists <- doesDirExist dataDir
  unless dataDirExists $ createDir dataDir
  input <- readFileBytes dataFile
  maybe (throwDatabase errUnableToDecode) (return . AppState.mkAppState) (Aeson.decode input)

initialize :: Config -> IO (Config, AppState)
initialize cfg = do
  state <- createState cfg
  return (cfg, state)

finalize :: (Config, AppState) -> IO ()
finalize _ = return ()

-- * Pre-Initialize

keyMapping :: [(Key, Key)]
keyMapping =
  [ ("Timestamp", "timestamp"),
    ("Id", "id"),
    ("KeyId", "keyId"),
    ("Description", "description"),
    ("Identity", "identity"),
    ("Ciphertext", "ciphertext"),
    ("Meta", "meta")
  ]

remapKeys :: [(Key, Key)] -> KeyMap Value -> KeyMap Value
remapKeys mapping = KeyMap.mapKeyVal f id
  where
    f k
      | Just mapped <- lookup k mapping = mapped
      | otherwise = k

remapJSON :: Value -> Value
remapJSON (Object obj) = Object (remapKeys keyMapping obj)
remapJSON (Array arr) = Array (fmap remapJSON arr)
remapJSON other = other

migrate :: (MonadAppError m, MonadFilesystem m) => Config -> SchemaVersion -> m ()
migrate cfg (MkSchemaVersion 2) = do
  let dataFile = configDataFile cfg
  jsonData <- readFileBytes dataFile
  decodedData <- maybe (throwMigration errUnableToDecode) return (Aeson.decode jsonData)
  let remappedData = remapJSON decodedData
  writeFileBytes dataFile . appendNewline . AesonPretty.encodePretty' aesonConfig $ remappedData
  return ()
migrate _ (MkSchemaVersion v) =
  throwMigration $ "no supported migration path for schema version " ++ show v

preInitialize ::
  (MonadAppError m, MonadFilesystem m, MonadInteraction m) =>
  Config ->
  SchemaVersion ->
  m ()
preInitialize cfg current = do
  let schemaFile = configSchemaFile cfg
  previous <- getSchemaVersion schemaFile
  if previous == current
    then return ()
    else do
      message $ mkMigrateMessage previous current
      migrate cfg previous
      _ <- createSchemaFile schemaFile current
      return ()

-- * Instances

instance MonadConfigReader JSON where
  askConfig = ask

instance MonadStore JSON where
  put = modify . AppState.put
  delete = modify . AppState.delete
  runQuery = gets . AppState.runQuery
  selectAll = gets AppState.selectAll
  getCount = gets AppState.getCount
  getCountOf = gets . AppState.getCountOf
