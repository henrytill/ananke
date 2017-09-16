{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Context
  ( AppContext(..)
  , AppConfig
  , _appConfigDataDirectory
  , _appConfigKeyId
  , getDataDir
  , configure
  ) where

import           Control.Monad.Except
import qualified Data.Map.Lazy          as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQLite
import           Lens.Simple
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Env       (getEnv)
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

getKeyId :: [(T.Text, TOML.Value)] -> Either String String
getKeyId tbl = maybe err pure keyId
  where
    keyId = alist tbl ^? mapAt "gnupg" . at "keyid" . _Just . _String . to T.unpack
    err   = Left "could not find gnupg.keyid"

getEnvOrDefault :: MonadIO m => String -> String -> m String
getEnvOrDefault env d = fromMaybe d <$> liftIO (getEnv env)

getEnvOrError :: MonadIO m => String -> String -> m String
getEnvOrError env msg = liftIO (getEnv env) >>= maybe (error msg) pure

getDataDir :: MonadIO m => m FilePath
getDataDir =
  getEnvOrError "HOME" "Can't find my way HOME" >>= \ home ->
  getEnvOrDefault "HECATE_DATA_DIR" (home ++ "/.hecate")

configure :: (MonadIO m, MonadError AppError m) => FilePath -> m AppConfig
configure dataDir = do
  txt   <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
  tbl   <- either (throwError . TomlParsing . show) pure (TOML.parseTOML txt)
  dfing <- either (throwError . TomlParsing)        pure (getKeyId tbl)
  keyId <- KeyId . T.pack <$> getEnvOrDefault "HECATE_KEYID" dfing
  let dbDir = dataDir ++ "/db"
  dbDirExists <- liftIO (doesDirectoryExist dbDir)
  unless dbDirExists (liftIO (createDirectory dbDir))
  return AppConfig { _appConfigDataDirectory = dataDir
                   , _appConfigKeyId = keyId
                   }
