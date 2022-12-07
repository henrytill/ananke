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

fromEnv :: MonadInteraction m => (String -> a) -> String -> m (Maybe a)
fromEnv f name = fmap (fmap f) (getEnv name)

fromPairs :: (String -> a) -> String -> Pairs -> Maybe a
fromPairs f name = fmap f . lookup name

createPreConfig :: MonadInteraction m => m PreConfig
createPreConfig = do
  dir     <- First <$> getEnv "HECATE_DATA_DIR"
  backend <- First <$> fromEnv mkBackend "HECATE_BACKEND"
  keyId   <- First <$> fromEnv mkKeyId   "HECATE_KEYID"
  mult    <- First <$> fromEnv mkBool    "HECATE_ALLOW_MULTIPLE_KEYS"
  return MkPreConfig { preConfigDataDirectory     = dir
                     , preConfigBackend           = backend
                     , preConfigKeyId             = keyId
                     , preConfigAllowMultipleKeys = mult
                     }

addDefaultConfig :: MonadInteraction m => PreConfig -> m PreConfig
addDefaultConfig preConfig = mappend preConfig <$> defaultConfig
  where
    getDefaultDataDirectory = case Info.os of
      "mingw32" -> fromEnv (++ "/hecate")  "APPDATA"
      _         -> fromEnv (++ "/.hecate") "HOME"
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
      txt   <- readFileAsString (dataDir ++ "/hecate.conf")
      pairs <- maybe (configurationError "Unable to parse hecate.conf") return (Parser.parse txt)
      let backend = First $ fromPairs mkBackend "backend"             pairs
          keyId   = First $ fromPairs mkKeyId   "keyid"               pairs
          mult    = First $ fromPairs mkBool    "allow_multiple_keys" pairs
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
