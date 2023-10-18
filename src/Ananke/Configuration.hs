{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ananke.Configuration
  ( Config(..)
  , Backend(..)
  , configureWith
  , configure
  ) where

import           Control.Monad (foldM)
import           Data.Bool     (bool)
import           Data.Ini      (Ini)
import qualified Data.Ini      as Ini
import           Data.Monoid   (First (..))
import           Data.String   (IsString)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Ananke.Class  (MonadAppError (..), MonadConfigure (..), MonadFilesystem (..))
import           Ananke.Data   (Backend (..), Config (..), KeyId (MkKeyId), PreConfig (..))


envConfigDirectory, envDataDirectory, envBackend, envKeyId, envAllowMultipleKeys :: String
envConfigDirectory   = "ANANKE_CONFIG_DIR"
envDataDirectory     = "ANANKE_DATA_DIR"
envBackend           = "ANANKE_BACKEND"
envKeyId             = "ANANKE_KEY_ID"
envAllowMultipleKeys = "ANANKE_ALLOW_MULTIPLE_KEYS"

errMsgConfigDirectory, errMsgDataDirectory, errMsgBackend, errMsgKeyId, errMsgAllowMultipleKeys :: String
errMsgConfigDirectory   = "Please set ANANKE_CONFIG_DIR"
errMsgDataDirectory     = "Please set ANANKE_DATA_DIR or data.dir in ananke.ini"
errMsgBackend           = "Please set ANANKE_BACKEND or data.backend in ananke.ini"
errMsgKeyId             = "Please set ANANKE_KEY_ID or gpg.key_id in ananke.ini"
errMsgAllowMultipleKeys = "Please set ANANKE_ALLOW_MULTIPLE_KEYS or data.allow_multiple_keys in ananke.ini"

errMsgConfigDirectoryDoesNotExist, errMsgUnableToParseIni :: String
errMsgConfigDirectoryDoesNotExist = "Configuration directory does not exist"
errMsgUnableToParseIni            = "Unable to parse ananke.ini"

iniSectionData, iniSectionGPG :: IsString a => a
iniSectionData = "data"
iniSectionGPG  = "gpg"

iniKeyDataDirectory, iniKeyBackend, iniKeyKeyId, iniKeyAllowMultipleKeys :: IsString a => a
iniKeyDataDirectory     = "dir"
iniKeyBackend           = "backend"
iniKeyKeyId             = "key_id"
iniKeyAllowMultipleKeys = "allow_multiple_keys"

anankeDirectory, dotAnankeDirectory :: FilePath
anankeDirectory    = "/ananke"
dotAnankeDirectory = "/.ananke"

anankeIniFile :: FilePath
anankeIniFile = "/ananke.ini"

trues :: [Text]
trues = [ "true", "t", "yes", "y", "1" ]

mkBackend :: Text -> Backend
mkBackend t = case T.toLower t of
  "sqlite" -> SQLite
  "json"   -> JSON
  _        -> SQLite

mkBool :: Text -> Bool
mkBool t | elem (T.toLower t) trues = True
         | otherwise                = False

fromEnv :: MonadConfigure m => (String -> a) -> String -> m (Maybe a)
fromEnv f name = fmap (fmap f) (getEnv name)

fromIni :: (Text -> a) -> Text -> Text -> Ini -> Maybe a
fromIni f section key = either (const Nothing) (Just . f) . Ini.lookupValue section key

-- | A configurator mappends a monadic action producing a PreConfig to the given PreConfig.
--
-- The action may also depend on the given PreConfig.
type Configurator m = PreConfig -> m PreConfig

configureFromEnvironment :: MonadConfigure m => Configurator m
configureFromEnvironment a = mappend a <$> b
  where
    b = do
      preConfigDirectory         <- First <$> getEnv envConfigDirectory
      preConfigDataDirectory     <- First <$> getEnv envDataDirectory
      preConfigBackend           <- First <$> fromEnv (mkBackend . T.pack) envBackend
      preConfigKeyId             <- First <$> fromEnv (MkKeyId   . T.pack) envKeyId
      preConfigAllowMultipleKeys <- First <$> fromEnv (mkBool    . T.pack) envAllowMultipleKeys
      return MkPreConfig { preConfigDirectory, preConfigDataDirectory, preConfigBackend, preConfigKeyId, preConfigAllowMultipleKeys }

configureConfigDirectory :: (MonadConfigure m, MonadFilesystem m) => Configurator m
configureConfigDirectory a = mappend a <$> b
  where
    b = do
      homeDirectory   <- getHomeDirectory
      configDirectory <- getConfigDirectory
      let homeDotAnankeDirectory = homeDirectory ++ dotAnankeDirectory
      preConfigDirectory <- First . Just . bool homeDotAnankeDirectory (configDirectory ++ anankeDirectory) <$> doesDirectoryExist configDirectory
      return mempty{preConfigDirectory}

configureFromFile :: (MonadAppError m, MonadConfigure m, MonadFilesystem m) => Configurator m
configureFromFile a = mappend a <$> b a
  where
    b pre = do
      configDirectory <- maybe (configurationError errMsgConfigDirectoryDoesNotExist) return . getFirst $ preConfigDirectory pre
      configText      <- readFileText $ configDirectory ++ anankeIniFile
      configIni       <- either (\s -> configurationError $ errMsgUnableToParseIni ++ ": " ++ s) return $ Ini.parseIni configText
      let preConfigDataDirectory     = First $ fromIni T.unpack  iniSectionData iniKeyDataDirectory     configIni
          preConfigBackend           = First $ fromIni mkBackend iniSectionData iniKeyBackend           configIni
          preConfigKeyId             = First $ fromIni MkKeyId   iniSectionGPG  iniKeyKeyId             configIni
          preConfigAllowMultipleKeys = First $ fromIni mkBool    iniSectionData iniKeyAllowMultipleKeys configIni
      return mempty{preConfigDataDirectory, preConfigBackend, preConfigKeyId, preConfigAllowMultipleKeys}

configureFromDefaults :: (MonadConfigure m, MonadFilesystem m) => Configurator m
configureFromDefaults a = mappend a <$> b
  where
    b = do
      homeDirectory <- getHomeDirectory
      dataDirectory <- getDataDirectory
      let homeDotAnankeDirectory = homeDirectory ++ dotAnankeDirectory
      preConfigDataDirectory <- First . Just . bool homeDotAnankeDirectory (dataDirectory ++ anankeDirectory) <$> doesDirectoryExist dataDirectory
      let preConfigAllowMultipleKeys = First . Just $ False
      return mempty{preConfigDataDirectory, preConfigAllowMultipleKeys}

mkConfig :: MonadAppError m => PreConfig -> m Config
mkConfig preConfig =
  MkConfig <$> firstOrError errMsgConfigDirectory   (preConfigDirectory         preConfig)
           <*> firstOrError errMsgDataDirectory     (preConfigDataDirectory     preConfig)
           <*> firstOrError errMsgBackend           (preConfigBackend           preConfig)
           <*> firstOrError errMsgKeyId             (preConfigKeyId             preConfig)
           <*> firstOrError errMsgAllowMultipleKeys (preConfigAllowMultipleKeys preConfig)
  where
    firstOrError msg = maybe (configurationError msg) pure . getFirst

configureWith :: forall m. (MonadAppError m, MonadConfigure m, MonadFilesystem m) => PreConfig -> m Config
configureWith overrides = foldM f overrides configurators >>= mkConfig
  where
    configurators :: [Configurator m]
    configurators = [ configureFromEnvironment
                    , configureConfigDirectory
                    , configureFromFile
                    , configureFromDefaults
                    ]

    f :: PreConfig -> Configurator m -> m PreConfig
    f acc configurator = configurator acc

configure :: (MonadAppError m, MonadConfigure m, MonadFilesystem m) => m Config
configure = configureWith mempty
