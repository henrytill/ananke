{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Hecate.Configuration
  ( Config(..)
  , Backend(..)
  , configureWith
  , configure
  ) where

import qualified Data.Maybe         as Maybe
import           Data.Monoid        (First (..))
import qualified Data.Text          as T
import           Lens.Family2
import           Lens.Family2.Stock (_Just)
import qualified System.Info        as Info
import qualified TOML
import           TOML.Lens

import           Hecate.Data
import           Hecate.Interfaces


mkBackend :: T.Text -> Backend
mkBackend txt = case T.toLower txt of
  "json" -> JSON
  _      -> SQLite

getDefaultDataDirectory :: MonadInteraction m => m (Maybe FilePath)
getDefaultDataDirectory = case Info.os of
  "mingw32" -> fmap (++ "/hecate")  <$> getEnv "APPDATA"
  _         -> fmap (++ "/.hecate") <$> getEnv "HOME"

getDataDirectoryFromEnv :: MonadInteraction m => m (Maybe FilePath)
getDataDirectoryFromEnv
  = getEnv "HECATE_DATA_DIR"

getBackendFromEnv :: MonadInteraction m => m (Maybe Backend)
getBackendFromEnv
  = fmap (mkBackend . T.pack) <$> getEnv "HECATE_BACKEND"

getKeyIdFromEnv :: MonadInteraction m => m (Maybe KeyId)
getKeyIdFromEnv
  = fmap (KeyId . T.pack) <$> getEnv "HECATE_KEYID"

getAllowMultipleKeysFromEnv :: MonadInteraction m => m (Maybe Bool)
getAllowMultipleKeysFromEnv
  = fmap f <$> getEnv "HECATE_ALLOW_MULTIPLE_KEYS"
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

getBackend :: [(T.Text, TOML.Value)] -> Maybe Backend
getBackend tbl
  = tbl ^? tableAt "core" . lup "backend" . _Just . _String . to mkBackend

getKeyId :: [(T.Text, TOML.Value)] -> Maybe KeyId
getKeyId tbl
  = tbl ^? tableAt "gnupg" . lup "keyid" . _Just . _String . to KeyId

getAllowMultipleKeys :: [(T.Text, TOML.Value)] -> Maybe Bool
getAllowMultipleKeys tbl
  = tbl ^? tableAt "entries" . lup "allow_multiple_keys" . _Just . _Bool

createPreConfig :: MonadInteraction m => m PreConfig
createPreConfig = do
  dir     <- First <$> getDataDirectoryFromEnv
  backend <- First <$> getBackendFromEnv
  keyId   <- First <$> getKeyIdFromEnv
  mult    <- First <$> getAllowMultipleKeysFromEnv
  return PreConfig { _preConfigDataDirectory     = dir
                   , _preConfigBackend           = backend
                   , _preConfigKeyId             = keyId
                   , _preConfigAllowMultipleKeys = mult
                   }

addDefaultConfig :: MonadInteraction m => PreConfig -> m PreConfig
addDefaultConfig preConfig = mappend preConfig <$> defaultConfig
  where
    defaultConfig = do
      dir <- First <$> getDefaultDataDirectory
      return PreConfig { _preConfigDataDirectory     = dir
                       , _preConfigBackend           = mempty
                       , _preConfigKeyId             = mempty
                       , _preConfigAllowMultipleKeys = First (Just False)
                       }

addTOMLConfig :: (MonadAppError m, MonadInteraction m) => PreConfig -> m PreConfig
addTOMLConfig preConfig = mappend preConfig <$> tomlConfig
  where
    tomlConfig = do
      let dataDir = Maybe.fromJust (getFirst (_preConfigDataDirectory preConfig))
      txt <- readFileAsText (dataDir ++ "/hecate.toml")
      tbl <- either tomlError pure (TOML.parseTOML txt)
      let backend = First (getBackend tbl)
          keyId   = First (getKeyId tbl)
          mult    = First (getAllowMultipleKeys tbl)
      return PreConfig { _preConfigDataDirectory     = mempty
                       , _preConfigBackend           = backend
                       , _preConfigKeyId             = keyId
                       , _preConfigAllowMultipleKeys = mult
                       }

preConfigToConfig :: MonadAppError m => PreConfig -> m Config
preConfigToConfig preConfig =
  Config <$> firstOrError dirMsg (_preConfigDataDirectory     preConfig)
         <*> firstOrError bakMsg (_preConfigBackend           preConfig)
         <*> firstOrError keyMsg (_preConfigKeyId             preConfig)
         <*> firstOrError mulMsg (_preConfigAllowMultipleKeys preConfig)
  where
    firstOrError msg = maybe (configurationError msg) pure . getFirst
    dirMsg = "Please set HECATE_DATA_DIR"
    bakMsg = "Please set HECATE_BACKEND or core.backend in hecate.toml"
    keyMsg = "Please set HECATE_KEYID or gnupg.keyid in hecate.toml"
    mulMsg = "Please set HECATE_ALLOW_MULTIPLE_KEYS or entries.allow_multiple_keys in hecate.toml"

configureWith :: (MonadAppError m, MonadInteraction m) => PreConfig -> m Config
configureWith preConfig = addDefaultConfig preConfig >>= addTOMLConfig >>= preConfigToConfig

configure :: (MonadAppError m, MonadInteraction m) => m Config
configure = createPreConfig >>= configureWith
