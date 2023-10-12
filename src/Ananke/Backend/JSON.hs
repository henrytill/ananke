{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}

module Ananke.Backend.JSON
  ( JSON
  , encodeJSON
  , run
  , preInitialize
  , initialize
  , finalize
  , AppState
  ) where

import           Control.Monad                (unless, when)
import           Control.Monad.Catch          (MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Reader         (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State          (MonadState, StateT, gets, modify, runStateT)
import           Data.Aeson                   (Value (..))
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as AesonPretty
import           Data.Aeson.KeyMap            (Key, KeyMap)
import qualified Data.Aeson.KeyMap            as KeyMap
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.List                    as List

import           Ananke.Backend
import           Ananke.Backend.JSON.AppState (AppState, appStateDirty)
import qualified Ananke.Backend.JSON.AppState as AppState
import           Ananke.Class
import           Ananke.Data                  (Config, Entry, SchemaVersion (..), configDataDirectory, configDataFile,
                                               configSchemaFile, entryKeyOrder)


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
           , MonadTime
           )

runJSON :: JSON a -> AppState -> Config -> IO (a, AppState)
runJSON m state cfg = runStateT (runReaderT (unJSON m) cfg) state

aesonConfig :: AesonPretty.Config
aesonConfig = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}

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
  let dataDir  = configDataDirectory cfg
      dataFile = configDataFile cfg
  dataDirExists <- doesDirectoryExist dataDir
  unless dataDirExists $ createDirectory dataDir
  input <- readFileBytes dataFile
  maybe (databaseError "unable to decode data.json") (return . AppState.mkAppState) (Aeson.decode input)

initialize :: Config -> IO (Config, AppState)
initialize cfg = do
  state <- createState cfg
  return (cfg, state)

finalize :: (Config, AppState) -> IO ()
finalize _ = return ()

-- * Pre-Initialize

keyMapping :: [(Key, Key)]
keyMapping =
  [ ("Timestamp", "timestamp")
  , ("Id", "id")
  , ("KeyId", "keyId")
  , ("Description", "description")
  , ("Identity", "identity")
  , ("Ciphertext", "ciphertext")
  , ("Meta", "meta")
  ]

remapKeys :: [(Key, Key)] -> KeyMap Value -> KeyMap Value
remapKeys mapping = KeyMap.mapKeyVal f id where
  f k | Just mapped <- lookup k mapping = mapped
      | otherwise                       = k

remapJSON :: Value -> Value
remapJSON (Object obj) = Object (remapKeys keyMapping obj)
remapJSON (Array arr)  = Array (fmap remapJSON arr)
remapJSON other        = other

migrate :: (MonadAppError m, MonadFilesystem m) => Config -> SchemaVersion -> m ()
migrate cfg (MkSchemaVersion 2) = do
  let dataFile = configDataFile cfg
  jsonData    <- readFileBytes dataFile
  decodedData <- maybe (migrationError "unable to decode data.json") return (Aeson.decode jsonData)
  let remappedData = remapJSON decodedData
  writeFileBytes dataFile . appendNewline . AesonPretty.encodePretty' aesonConfig $ remappedData
  return ()
migrate _ (MkSchemaVersion v) =
  migrationError $ "no supported migration path for schema version " ++ show v

preInitialize :: Config -> IO ()
preInitialize cfg = do
  let schemaFile = configSchemaFile cfg
  schemaVersion <- getSchemaVersion schemaFile
  if schemaVersion == currentSchemaVersion
    then return ()
    else do putStrLn ("Migrating data file from schema version "
                      ++ show schemaVersion
                      ++ " to version "
                      ++ show currentSchemaVersion
                      ++ "...")
            migrate cfg schemaVersion
            _ <- createSchemaFile schemaFile currentSchemaVersion
            return ()

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
