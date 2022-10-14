module Hecate.Configuration
  ( Config(..)
  , Backend(..)
  , configureWith
  , configure
  ) where

import           Data.Char                   (toLower)
import qualified Data.Maybe                  as Maybe
import           Data.Monoid                 (First (..))
import qualified Data.Text                   as T
import qualified System.Info                 as Info

import           Hecate.Configuration.Parser (Pairs)
import qualified Hecate.Configuration.Parser as Parser
import           Hecate.Data
import           Hecate.Interfaces


mkBackend :: String -> Backend
mkBackend txt = case toLower <$> txt of
  "sqlite" -> SQLite
  "json"   -> JSON
  _        -> SQLite

mkKeyId :: String -> KeyId
mkKeyId = MkKeyId . T.pack

mkBool :: String -> Bool
mkBool "True" = True
mkBool "true" = True
mkBool "TRUE" = True
mkBool "t"    = True
mkBool "T"    = True
mkBool "1"    = True
mkBool _      = False

getDataDirectoryFromEnv     :: MonadInteraction m => m (Maybe FilePath)
getBackendFromEnv           :: MonadInteraction m => m (Maybe Backend)
getKeyIdFromEnv             :: MonadInteraction m => m (Maybe KeyId)
getAllowMultipleKeysFromEnv :: MonadInteraction m => m (Maybe Bool)
getDataDirectoryFromEnv     = getEnv "HECATE_DATA_DIR"
getBackendFromEnv           = fmap mkBackend <$> getEnv "HECATE_BACKEND"
getKeyIdFromEnv             = fmap mkKeyId   <$> getEnv "HECATE_KEYID"
getAllowMultipleKeysFromEnv = fmap mkBool    <$> getEnv "HECATE_ALLOW_MULTIPLE_KEYS"

getBackend           :: Pairs -> Maybe Backend
getKeyId             :: Pairs -> Maybe KeyId
getAllowMultipleKeys :: Pairs -> Maybe Bool
getBackend           = fmap mkBackend . lookup "backend"
getKeyId             = fmap mkKeyId   . lookup "keyid"
getAllowMultipleKeys = fmap mkBool    . lookup "allow_multiple_keys"

createPreConfig :: MonadInteraction m => m PreConfig
createPreConfig = do
  dir     <- First <$> getDataDirectoryFromEnv
  backend <- First <$> getBackendFromEnv
  keyId   <- First <$> getKeyIdFromEnv
  mult    <- First <$> getAllowMultipleKeysFromEnv
  return MkPreConfig { preConfigDataDirectory     = dir
                     , preConfigBackend           = backend
                     , preConfigKeyId             = keyId
                     , preConfigAllowMultipleKeys = mult
                     }

addDefaultConfig :: MonadInteraction m => PreConfig -> m PreConfig
addDefaultConfig preConfig = mappend preConfig <$> defaultConfig
  where
    getDefaultDataDirectory = case Info.os of
      "mingw32" -> fmap (++ "/hecate")  <$> getEnv "APPDATA"
      _         -> fmap (++ "/.hecate") <$> getEnv "HOME"
    defaultConfig = do
      dir <- First <$> getDefaultDataDirectory
      return MkPreConfig { preConfigDataDirectory     = dir
                         , preConfigBackend           = mempty
                         , preConfigKeyId             = mempty
                         , preConfigAllowMultipleKeys = First (Just False)
                         }

addFileConfig :: (MonadAppError m, MonadInteraction m) => PreConfig -> m PreConfig
addFileConfig preConfig = mappend preConfig <$> fileConfig
  where
    fileConfig = do
      let dataDir = Maybe.fromJust (getFirst (preConfigDataDirectory preConfig))
      txt <- readFileAsString (dataDir ++ "/hecate.conf")
      tbl <- maybe (configurationError "Unable to parse hecate.conf") return (Parser.parse txt)
      let backend = First (getBackend tbl)
          keyId   = First (getKeyId tbl)
          mult    = First (getAllowMultipleKeys tbl)
      return MkPreConfig { preConfigDataDirectory     = mempty
                         , preConfigBackend           = backend
                         , preConfigKeyId             = keyId
                         , preConfigAllowMultipleKeys = mult
                         }

preConfigToConfig :: MonadAppError m => PreConfig -> m Config
preConfigToConfig preConfig =
  MkConfig <$> firstOrError dirMsg (preConfigDataDirectory     preConfig)
           <*> firstOrError bakMsg (preConfigBackend           preConfig)
           <*> firstOrError keyMsg (preConfigKeyId             preConfig)
           <*> firstOrError mulMsg (preConfigAllowMultipleKeys preConfig)
  where
    firstOrError msg = maybe (configurationError msg) pure . getFirst
    dirMsg = "Please set HECATE_DATA_DIR"
    bakMsg = "Please set HECATE_BACKEND or backend in hecate.conf"
    keyMsg = "Please set HECATE_KEYID or keyid in hecate.conf"
    mulMsg = "Please set HECATE_ALLOW_MULTIPLE_KEYS or allow_multiple_keys in hecate.conf"

configureWith :: (MonadAppError m, MonadInteraction m) => PreConfig -> m Config
configureWith preConfig = addDefaultConfig preConfig >>= addFileConfig >>= preConfigToConfig

configure :: (MonadAppError m, MonadInteraction m) => m Config
configure = createPreConfig >>= configureWith
