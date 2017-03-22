{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.IO.Config where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Hecate.Types (AppConfig(..), AppError(..), KeyId(..))
import System.Directory (createDirectory, doesDirectoryExist)
import System.Posix.Env (getEnv)
import Text.Toml
import Text.Toml.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

lup :: HM.HashMap T.Text v -> T.Text -> Either AppError v
lup hm key = maybe err pure (HM.lookup key hm)
  where
    err = Left (TomlParsing ("could't find key: " ++ T.unpack key))

parseKeyId :: Table -> Either AppError String
parseKeyId tbl = unpackTop tbl >>= unpackRecipient >>= unpackKeyId
  where
    unpackTop t = lup t "recipient"

    unpackRecipient (VTable r) = lup r "keyid"
    unpackRecipient _          = Left (TomlParsing "recipient is wrong type")

    unpackKeyId (VString f) = pure (T.unpack f)
    unpackKeyId _           = Left (TomlParsing "keyid is wrong type")

getEnvOrDefault :: MonadIO m => String -> String -> m String
getEnvOrDefault env d = fromMaybe d <$> liftIO (getEnv env)

configure :: (MonadIO m, MonadError AppError m) => m AppConfig
configure = do
  home      <- liftIO (getEnv "HOME") >>= maybe (error "Can't find my way HOME") pure
  dataDir   <- getEnvOrDefault "HECATE_DATA_DIR" (home ++ "/.hecate")
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
