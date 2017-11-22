{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Hecate.Context
  ( HasConfig
    ( configDataDirectory
    , configDatabaseDirectory
    , configSchemaFile
    , configDatabaseFile
    , configKeyId
    , configAllowMultipleKeys
    )
  , HasAppContext
    ( appContextConfig
    , appContextConnection
    )
  , PreConfig(..)
  , Config
  , configureWith
  , configure
  , AppContext
  , createContext
  , finalize
  ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Maybe             (fromJust)
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2
import           Lens.Family2.Stock     (_Just)
import           Lens.Family2.Unchecked (lens)
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Env       (getEnv)
import qualified TOML
import           TOML.Lens

import           Hecate.Error
import           Hecate.GPG             (KeyId(..))


class HasConfig t where
  config                  :: Lens' t Config
  configDataDirectory     :: Lens' t FilePath
  configKeyId             :: Lens' t KeyId
  configAllowMultipleKeys :: Lens' t Bool
  configDatabaseDirectory :: Getter' t FilePath
  configSchemaFile        :: Getter' t FilePath
  configDatabaseFile      :: Getter' t FilePath
  configDataDirectory     = config . configDataDirectory
  configKeyId             = config . configKeyId
  configAllowMultipleKeys = config . configAllowMultipleKeys
  configDatabaseDirectory = config . configDatabaseDirectory
  configSchemaFile        = config . configSchemaFile
  configDatabaseFile      = config . configDatabaseFile

class HasConfig t => HasAppContext t where
  appContext           :: Lens' t AppContext
  appContextConfig     :: Lens' t Config
  appContextConnection :: Lens' t SQLite.Connection
  appContextConfig     = appContext . appContextConfig
  appContextConnection = appContext . appContextConnection

-- | A 'Config' represents our application's configuration
data Config = Config
  { _configDataDirectory     :: FilePath
  , _configKeyId             :: KeyId
  , _configAllowMultipleKeys :: Bool
  } deriving (Show, Eq)

instance HasConfig Config where
  config                  = id
  configDataDirectory     = lens _configDataDirectory     (\ c dir      -> c{_configDataDirectory     = dir})
  configKeyId             = lens _configKeyId             (\ c keyId    -> c{_configKeyId             = keyId})
  configAllowMultipleKeys = lens _configAllowMultipleKeys (\ c multKeys -> c{_configAllowMultipleKeys = multKeys})
  configDatabaseDirectory = configDataDirectory     . to (++ "/db")
  configSchemaFile        = configDatabaseDirectory . to (++ "/schema")
  configDatabaseFile      = configDatabaseDirectory . to (++ "/db.sqlite")

-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { _appContextConfig     :: Config
  , _appContextConnection :: SQLite.Connection
  }

instance HasConfig AppContext where
  config = lens _appContextConfig (\ a cfg -> a{_appContextConfig = cfg})

instance HasAppContext AppContext where
  appContext           = id
  appContextConfig     = config
  appContextConnection = lens _appContextConnection (\ a conn -> a{_appContextConnection = conn})


getHomeFromEnv :: MonadIO m => m (Maybe FilePath)
getHomeFromEnv
  = liftIO (getEnv "HOME")

getDataDirectoryFromEnv :: MonadIO m => m (Maybe FilePath)
getDataDirectoryFromEnv
  = liftIO (getEnv "HECATE_DATA_DIR")

getKeyIdFromEnv :: MonadIO m => m (Maybe KeyId)
getKeyIdFromEnv
  = fmap (KeyId . T.pack) <$> liftIO (getEnv "HECATE_KEYID")

getAllowMultipleKeysFromEnv :: MonadIO m => m (Maybe Bool)
getAllowMultipleKeysFromEnv
  = fmap f <$> liftIO (getEnv "HECATE_ALLOW_MULTIPLE_KEYS")
  where
    f "1" = True
    f "0" = False
    f _   = False

lup
  :: T.Text
  -> Getter' [(T.Text, TOML.Value)] (Maybe TOML.Value)
lup = to . lookup

tableAt
  :: (Applicative f, Phantom f)
  => T.Text
  -> ([(T.Text, TOML.Value)] -> f [(T.Text, TOML.Value)])
  -> [(T.Text, TOML.Value)]
  -> f [(T.Text, TOML.Value)]
tableAt k = lup k . _Just . _Table

getKeyId :: [(T.Text, TOML.Value)] -> Maybe KeyId
getKeyId tbl
  = tbl ^? tableAt "gnupg" . lup "keyid" . _Just . _String . to KeyId

getAllowMultipleKeys :: [(T.Text, TOML.Value)] -> Maybe Bool
getAllowMultipleKeys tbl
  = tbl ^? tableAt "entries" . lup "allow_multiple_keys" . _Just . _Bool


-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = PreConfig
  { _preConfigDataDirectory     :: First FilePath
  , _preConfigKeyId             :: First KeyId
  , _preConfigAllowMultipleKeys :: First Bool
  } deriving (Show, Eq)

instance Monoid PreConfig where
  mempty
    = PreConfig mempty mempty mempty

  PreConfig a b c `mappend` PreConfig d e f
    = PreConfig (a `mappend` d)
                (b `mappend` e)
                (c `mappend` f)

createPreConfig :: MonadIO m => m PreConfig
createPreConfig = do
  dir   <- First <$> getDataDirectoryFromEnv
  keyId <- First <$> getKeyIdFromEnv
  mult  <- First <$> getAllowMultipleKeysFromEnv
  return PreConfig { _preConfigDataDirectory     = dir
                   , _preConfigKeyId             = keyId
                   , _preConfigAllowMultipleKeys = mult
                   }

addDefaultConfig :: MonadIO m => PreConfig -> m PreConfig
addDefaultConfig preConfig = mappend <$> pure preConfig <*> defaultConfig
  where
    defaultConfig = do
      dir <- fmap (++ "/.hecate") <$> (First <$> getHomeFromEnv)
      return PreConfig { _preConfigDataDirectory     = dir
                       , _preConfigKeyId             = mempty
                       , _preConfigAllowMultipleKeys = First (Just False)
                       }

addTOMLConfig :: (MonadThrow m, MonadIO m) => PreConfig -> m PreConfig
addTOMLConfig preConfig = mappend <$> pure preConfig <*> tomlConfig
  where
    tomlConfig = do
      let dataDir = fromJust (getFirst (_preConfigDataDirectory preConfig))
      txt <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
      tbl <- either (throwM . TOML) pure (TOML.parseTOML txt)
      let keyId = First (getKeyId tbl)
          mult  = First (getAllowMultipleKeys tbl)
      return PreConfig { _preConfigDataDirectory     = mempty
                       , _preConfigKeyId             = keyId
                       , _preConfigAllowMultipleKeys = mult
                       }

preConfigToConfig :: MonadThrow m => PreConfig -> m Config
preConfigToConfig preConfig =
  Config <$> firstOrError dirMsg (_preConfigDataDirectory     preConfig)
         <*> firstOrError keyMsg (_preConfigKeyId             preConfig)
         <*> firstOrError mulMsg (_preConfigAllowMultipleKeys preConfig)
  where
    firstOrError msg = maybe (throwM (Configuration msg)) pure . getFirst
    dirMsg = "Please set HECATE_DATA_DIR"
    keyMsg = "Please set HECATE_KEYID or gnupg.keyid in hecate.toml"
    mulMsg = "Please set HECATE_ALLOW_MULTIPLE_KEYS or entries.allow_multiple_keys in hecate.toml"

configureWith :: (MonadThrow m, MonadIO m) => PreConfig -> m Config
configureWith preConfig = addDefaultConfig preConfig >>= addTOMLConfig >>= preConfigToConfig

configure :: (MonadThrow m, MonadIO m) => m Config
configure = createPreConfig >>= configureWith

createContext :: MonadIO m => Config -> m AppContext
createContext cfg = do
  let dbDir  = cfg ^. configDatabaseDirectory
      dbFile = cfg ^. configDatabaseFile
  dbDirExists <- liftIO (doesDirectoryExist dbDir)
  unless dbDirExists (liftIO (createDirectory dbDir))
  AppContext cfg <$> liftIO (SQLite.open dbFile)

finalize :: MonadIO m => AppContext -> m ()
finalize ctx = liftIO (SQLite.close conn)
  where
    conn = ctx ^. appContextConnection
