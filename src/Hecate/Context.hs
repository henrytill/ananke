{-# LANGUAGE OverloadedStrings #-}

module Hecate.Context
  ( AppContext(..)
  , HasAppContext
  , appContext
  , appContextKeyId
  , appContextConnection
  , AppConfig
  , _appConfigDataDirectory
  , _appConfigKeyId
  , getDataDir
  , configure
  ) where

import           Control.Exception
import           Control.Monad.Except
import qualified Data.Map.Lazy          as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQLite
import           Lens.Simple
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Env       (getEnv, getEnvDefault)
import qualified TOML
import           TOML.Lens

import           Hecate.Error
import           Hecate.GPG             (KeyId(..))

-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { _appContextKeyId      :: KeyId
  , _appContextConnection :: SQLite.Connection
  }

class HasAppContext t where
  appContext           :: Lens' t AppContext
  appContextKeyId      :: Lens' t KeyId
  appContextConnection :: Lens' t SQLite.Connection
  appContextKeyId      = appContext . appContextKeyId
  appContextConnection = appContext . appContextConnection

instance HasAppContext AppContext where
  appContext           = id
  appContextKeyId      = lens _appContextKeyId      (\ a k -> a{_appContextKeyId      = k})
  appContextConnection = lens _appContextConnection (\ a c -> a{_appContextConnection = c})

-- | An 'AppConfig' represents values read from a configuration file
data AppConfig = AppConfig
  { _appConfigDataDirectory :: FilePath
  , _appConfigKeyId         :: KeyId
  } deriving (Show, Eq)

alist :: Ord k => [(k, v)] -> Map.Map k v
alist = Map.fromList

alistLens
  :: (Ord k1, Ord k2, Functor f)
  => LensLike f [(k1, v1)] [(k2, v2)] (Map.Map k1 v1) (Map.Map k2 v2)
alistLens = iso Map.fromList Map.toList

mapAt
  :: Applicative f
  => T.Text
  -> (Map.Map T.Text TOML.Value -> f (Map.Map T.Text TOML.Value))
  -> Map.Map T.Text TOML.Value
  -> f (Map.Map T.Text TOML.Value)
mapAt k = at k . _Just . _Table . alistLens

getKeyId :: [(T.Text, TOML.Value)] -> Either String T.Text
getKeyId tbl = maybe err pure keyId
  where
    keyId = alist tbl ^? mapAt "gnupg" . at "keyid" . _Just . _String
    err   = Left "could not find gnupg.keyid"


getEnvError :: MonadIO m => String -> String -> m String
getEnvError env msg =  maybe (error msg) pure =<< liftIO (getEnv env)

getDataDir :: MonadIO m => m FilePath
getDataDir =
  getEnvError "HOME" "Could not get value of HOME" >>= \ home ->
  liftIO (getEnvDefault "HECATE_DATA_DIR" (home ++ "/.hecate"))

configure :: MonadIO m => FilePath -> m AppConfig
configure dataDir = do
  txt   <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
  tbl   <- either (throw . TOML)          pure (TOML.parseTOML txt)
  dfing <- either (throw . Configuration) pure (getKeyId tbl)
  keyId <- pure . KeyId <$> maybe dfing T.pack =<< liftIO (getEnv "HECATE_KEYID")
  let dbDir = dataDir ++ "/db"
  dbDirExists <- liftIO (doesDirectoryExist dbDir)
  unless dbDirExists (liftIO (createDirectory dbDir))
  return AppConfig { _appConfigDataDirectory = dataDir
                   , _appConfigKeyId = keyId
                   }
