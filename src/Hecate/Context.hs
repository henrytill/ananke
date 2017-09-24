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
  , Config
  , AppContext
  , getDataDir
  , configure
  , createContext
  , finalize
  ) where

import           Control.Exception
import           Control.Monad.Except
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2
import           Lens.Family2.Stock     (_Just)
import           Lens.Family2.Unchecked (lens)
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Env       (getEnv, getEnvDefault)
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

-- | An 'Config' represents values read from a configuration file
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

getKeyId :: [(T.Text, TOML.Value)] -> Either String T.Text
getKeyId tbl = maybe err pure keyId
  where
    keyId = tbl ^? tableAt "gnupg" . lup "keyid" . _Just . _String
    err   = Left "could not find gnupg.keyid"

getAllowMultipleKeys :: [(T.Text, TOML.Value)] -> Either String Bool
getAllowMultipleKeys tbl = maybe err pure allowMultKeys
  where
    allowMultKeys = tbl ^? tableAt "entries" . lup "allow_multiple_keys" . _Just . _Bool
    err           = Left "could not find entries.allow_multiple_keys"

getEnvError :: MonadIO m => String -> String -> m String
getEnvError env msg =  maybe (error msg) pure =<< liftIO (getEnv env)

getDataDir :: MonadIO m => m FilePath
getDataDir =
  getEnvError "HOME" "Could not get value of HOME" >>= \ home ->
  liftIO (getEnvDefault "HECATE_DATA_DIR" (home ++ "/.hecate"))

configure :: MonadIO m => FilePath -> m Config
configure dataDir = do
  txt   <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
  tbl   <- either (throw . TOML)          pure (TOML.parseTOML txt)
  dfing <- either (throw . Configuration) pure (getKeyId tbl)
  mult  <- either (throw . Configuration) pure (getAllowMultipleKeys tbl)
  keyId <- pure . KeyId <$> maybe dfing T.pack =<< liftIO (getEnv "HECATE_KEYID")
  return Config { _configDataDirectory     = dataDir
                , _configKeyId             = keyId
                , _configAllowMultipleKeys = mult
                }

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
