{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Context
  ( AppContext(..)
  , AppConfig
  , appConfigDataDirectory
  , appConfigKeyId
  , getDataDir
  , configure
  ) where

import           Control.Monad.Except
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQLite
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Env       (getEnv)
import qualified TOML

import           Hecate.Error
import           Hecate.GPG             (KeyId(..))

-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { appContextKeyId      :: KeyId
  , appContextConnection :: SQLite.Connection
  }

-- | An 'AppConfig' represents values read from a configuration file
data AppConfig = AppConfig
  { appConfigDataDirectory :: FilePath
  , appConfigKeyId         :: KeyId
  } deriving (Show, Eq)

-- | Look up values in a given association list
lup :: [(T.Text, TOML.Value)] -> T.Text -> Either AppError TOML.Value
lup alist key = maybe err pure (lookup key alist)
  where
    err = Left (TomlParsing ("could't find key: " ++ T.unpack key))

parseKeyId :: [(T.Text, TOML.Value)] -> Either AppError String
parseKeyId tbl = unpackTop tbl >>= unpackGnupg >>= unpackKeyId
  where
    unpackTop t = lup t "gnupg"

    unpackGnupg (TOML.Table r) = lup r "keyid"
    unpackGnupg _              = Left (TomlParsing "gnupg is wrong type")

    unpackKeyId (TOML.String f) = pure (T.unpack f)
    unpackKeyId _               = Left (TomlParsing "keyid is wrong type")

getEnvOrDefault :: MonadIO m => String -> String -> m String
getEnvOrDefault env d = fromMaybe d <$> liftIO (getEnv env)

getEnvOrError :: MonadIO m => String -> String -> m String
getEnvOrError env msg = liftIO (getEnv env) >>= maybe (error msg) pure

getDataDir :: MonadIO m => m FilePath
getDataDir =
  getEnvOrError "HOME" "Can't find my way HOME" >>= \home ->
  getEnvOrDefault "HECATE_DATA_DIR" (home ++ "/.hecate")

configure :: (MonadIO m, MonadError AppError m) => FilePath -> m AppConfig
configure dataDir = do
  txt   <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
  tbl   <- either (throwError . TomlParsing . show) pure (TOML.parseTOML txt)
  dfing <- either throwError pure (parseKeyId tbl)
  keyId <- KeyId . T.pack <$> getEnvOrDefault "HECATE_KEYID" dfing
  let dbDir = dataDir ++ "/db"
  dbDirExists <- liftIO (doesDirectoryExist dbDir)
  unless dbDirExists (liftIO (createDirectory dbDir))
  return AppConfig { appConfigDataDirectory = dataDir
                   , appConfigKeyId = keyId
                   }
