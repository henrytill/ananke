{-# LANGUAGE OverloadedStrings #-}

module Ananke.Configuration
  ( Config(..)
  , Backend(..)
  , configureWith
  , configure
  ) where

import           Data.Ini     (Ini)
import qualified Data.Ini     as Ini
import qualified Data.Maybe   as Maybe
import           Data.Monoid  (First (..))
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified System.Info  as Info

import           Ananke.Class (MonadAppError (..), MonadConfigure (..))
import           Ananke.Data


mkBackend :: Text -> Backend
mkBackend t = case T.toLower t of
  "sqlite" -> SQLite
  "json"   -> JSON
  _        -> SQLite

mkKeyId :: Text -> KeyId
mkKeyId = MkKeyId

trues :: [Text]
trues = [ "true", "t", "yes", "y", "1" ]

mkBool :: Text -> Bool
mkBool t | elem (T.toLower t) trues = True
         | otherwise                = False

fromEnv :: MonadConfigure m => (String -> a) -> String -> m (Maybe a)
fromEnv f name = fmap (fmap f) (getEnv name)

fromIni :: (Text -> a) -> Text -> Text -> Ini -> Maybe a
fromIni f section key = either (const Nothing) (Just . f) . Ini.lookupValue section key

createPreConfig :: MonadConfigure m => m PreConfig
createPreConfig = do
  dir     <- First <$> getEnv "ANANKE_DATA_DIR"
  backend <- First <$> fromEnv (mkBackend . T.pack) "ANANKE_BACKEND"
  keyId   <- First <$> fromEnv (mkKeyId   . T.pack) "ANANKE_KEYID"
  mult    <- First <$> fromEnv (mkBool    . T.pack) "ANANKE_ALLOW_MULTIPLE_KEYS"
  return MkPreConfig { preConfigDataDirectory     = dir
                     , preConfigBackend           = backend
                     , preConfigKeyId             = keyId
                     , preConfigAllowMultipleKeys = mult
                     }

addDefaultConfig :: MonadConfigure m => PreConfig -> m PreConfig
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

addFileConfig :: (MonadAppError m, MonadConfigure m) => PreConfig -> m PreConfig
addFileConfig preConfig = mappend preConfig <$> fileConfig
  where
    fileConfig = do
      let dataDir = Maybe.fromJust (getFirst (preConfigDataDirectory preConfig))
      txt <- readConfigFile (dataDir ++ "/ananke.ini")
      ini <- either (const . configurationError $ "Unable to parse ananke.ini") return (Ini.parseIni txt)
      let backend = First $ fromIni mkBackend "data" "backend"             ini
          mult    = First $ fromIni mkBool    "data" "allow_multiple_keys" ini
          keyId   = First $ fromIni mkKeyId   "gpg"  "key_id"              ini
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

configureWith :: (MonadAppError m, MonadConfigure m) => PreConfig -> m Config
configureWith preConfig = addDefaultConfig preConfig >>= addFileConfig >>= preConfigToConfig

configure :: (MonadAppError m, MonadConfigure m) => m Config
configure = createPreConfig >>= configureWith
