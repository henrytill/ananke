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

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Posix.Env (getEnv)
import Text.Toml
import Text.Toml.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQLite

import Hecate.Error
import Hecate.GPG (KeyId(..))

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

-- | Look up values in a given HashMap
lup :: HM.HashMap T.Text v -> T.Text -> Either AppError v
lup hm key = maybe err pure (HM.lookup key hm)
  where
    err = Left (TomlParsing ("could't find key: " ++ T.unpack key))

parseKeyId :: Table -> Either AppError String
parseKeyId tbl = unpackTop tbl >>= unpackGnupg >>= unpackKeyId
  where
    unpackTop t = lup t "gnupg"

    unpackGnupg (VTable r) = lup r "keyid"
    unpackGnupg _          = Left (TomlParsing "gnupg is wrong type")

    unpackKeyId (VString f) = pure (T.unpack f)
    unpackKeyId _           = Left (TomlParsing "keyid is wrong type")

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
  dirExists <- liftIO (doesDirectoryExist dataDir)
  unless dirExists (liftIO (createDirectory dataDir))
  unless dirExists (liftIO (createDirectory (dataDir ++ "/db")))
  txt       <- liftIO (TIO.readFile (dataDir ++ "/hecate.toml"))
  tbl       <- either (throwError . TomlParsing . show) pure (parseTomlDoc "" txt)
  dfing     <- either throwError pure (parseKeyId tbl)
  keyId     <- KeyId . T.pack <$> getEnvOrDefault "HECATE_KEYID" dfing
  return AppConfig { appConfigDataDirectory = dataDir
                   , appConfigKeyId = keyId
                   }
