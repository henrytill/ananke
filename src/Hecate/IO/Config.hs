{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.IO.Config where

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Hecate.Types (AppConfig(..), AppError(..), Fingerprint(..))
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

parseFingerprint :: Table -> Either AppError String
parseFingerprint tbl = unpackTop tbl >>= unpackRecipient >>= unpackFingerprint
  where
    unpackTop t = lup t "recipient"

    unpackRecipient (VTable r) = lup r "fingerprint"
    unpackRecipient _          = Left (TomlParsing "recipient is wrong type")

    unpackFingerprint (VString f) = pure (T.unpack f)
    unpackFingerprint _           = Left (TomlParsing "fingerprint is wrong type")

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
  dfing     <- either throwError pure (parseFingerprint tbl)
  fprint    <- Fingerprint . T.pack <$> getEnvOrDefault "HECATE_FINGERPRINT" dfing
  return AppConfig { appConfigDataDirectory = dataDir
                   , appConfigFingerprint = fprint
                   }
