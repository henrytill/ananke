module Ananke.Configuration
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

import           Ananke.Configuration.Parser (Pairs)
import qualified Ananke.Configuration.Parser as Parser
import           Ananke.Data
import           Ananke.Interfaces


mkBackend :: String -> Backend
mkBackend s = case toLower <$> s of
  "sqlite" -> SQLite
  "json"   -> JSON
  _        -> SQLite

mkKeyId :: String -> KeyId
mkKeyId = MkKeyId . T.pack

trues :: [String]
trues = ["true", "t", "yes", "y", "1"]

mkBool :: String -> Bool
mkBool s | elem (toLower <$> s) trues = True
         | otherwise                  = False

fromEnv :: MonadInteraction m => (String -> a) -> String -> m (Maybe a)
fromEnv f name = fmap (fmap f) (getEnv name)

fromPairs :: (String -> a) -> String -> Pairs -> Maybe a
fromPairs f name = fmap f . lookup name

createPreConfig :: MonadInteraction m => m PreConfig
createPreConfig = do
  dir     <- First <$> getEnv "ANANKE_DATA_DIR"
  backend <- First <$> fromEnv mkBackend "ANANKE_BACKEND"
  keyId   <- First <$> fromEnv mkKeyId   "ANANKE_KEYID"
  mult    <- First <$> fromEnv mkBool    "ANANKE_ALLOW_MULTIPLE_KEYS"
  return MkPreConfig { preConfigDataDirectory     = dir
                     , preConfigBackend           = backend
                     , preConfigKeyId             = keyId
                     , preConfigAllowMultipleKeys = mult
                     }

addDefaultConfig :: MonadInteraction m => PreConfig -> m PreConfig
addDefaultConfig preConfig = mappend preConfig <$> defaultConfig
  where
    getDefaultDataDirectory = case Info.os of
      "mingw32" -> fromEnv (++ "/ananke")  "APPDATA"
      _         -> fromEnv (++ "/.ananke") "HOME"
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
      txt   <- readFileAsString (dataDir ++ "/ananke.conf")
      pairs <- maybe (configurationError "Unable to parse ananke.conf") return (Parser.parse txt)
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
    dirMsg = "Please set ANANKE_DATA_DIR"
    bakMsg = "Please set ANANKE_BACKEND or backend in ananke.conf"
    keyMsg = "Please set ANANKE_KEYID or keyid in ananke.conf"
    mulMsg = "Please set ANANKE_ALLOW_MULTIPLE_KEYS or allow_multiple_keys in ananke.conf"

configureWith :: (MonadAppError m, MonadInteraction m) => PreConfig -> m Config
configureWith preConfig = addDefaultConfig preConfig >>= addFileConfig >>= preConfigToConfig

configure :: (MonadAppError m, MonadInteraction m) => m Config
configure = createPreConfig >>= configureWith
