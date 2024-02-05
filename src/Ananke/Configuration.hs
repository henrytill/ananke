{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ananke.Configuration
  ( Config (..),
    Backend (..),
    configureWith,
    configure,
  )
where

import Ananke.Class (MonadAppError (..), MonadConfigure (..), MonadFilesystem (..))
import Ananke.Data (Backend (..), Config (..), KeyId (MkKeyId), PreConfig (..))
import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Ini (Ini)
import Data.Ini qualified as Ini
import Data.Monoid (First (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath ((<.>), (</>))

envConfigDir, envDataDir, envBackend, envKeyId, envMultKeys :: String
envConfigDir = "ANANKE_CONFIG_DIR"
envDataDir = "ANANKE_DATA_DIR"
envBackend = "ANANKE_BACKEND"
envKeyId = "ANANKE_KEY_ID"
envMultKeys = "ANANKE_ALLOW_MULTIPLE_KEYS"

errConfigDir, errDataDir, errBackend, errKeyId, errMultKeys :: String
errConfigDir = "please set ANANKE_CONFIG_DIR"
errDataDir = "please set ANANKE_DATA_DIR or data.dir in ananke.ini"
errBackend = "please set ANANKE_BACKEND or data.backend in ananke.ini"
errKeyId = "please set ANANKE_KEY_ID or gpg.key_id in ananke.ini"
errMultKeys = "please set ANANKE_ALLOW_MULTIPLE_KEYS or data.allow_multiple_keys in ananke.ini"

errNoConfigDir :: String
errNoConfigDir = "no configuration directory specified"

iniSectionData, iniSectionGPG :: (IsString a) => a
iniSectionData = "data"
iniSectionGPG = "gpg"

iniKeyDataDir, iniKeyBackend, iniKeyKeyId, iniKeyMultKeys :: (IsString a) => a
iniKeyDataDir = "dir"
iniKeyBackend = "backend"
iniKeyKeyId = "key_id"
iniKeyMultKeys = "allow_multiple_keys"

anankeDir, dotAnankeDir :: FilePath
anankeDir = "ananke"
dotAnankeDir = ".ananke"

anankeIniFile :: FilePath
anankeIniFile = "ananke" <.> "ini"

mkBackend :: Text -> Backend
mkBackend t = case T.toLower t of
  "sqlite" -> SQLite
  "json" -> JSON
  _ -> SQLite

mkBool :: Text -> Bool
mkBool t
  | elem (T.toLower t) trues = True
  | otherwise = False
  where
    trues :: [Text]
    trues = ["true", "t", "yes", "y", "1"]

fromEnv :: (MonadConfigure m) => (String -> a) -> String -> m (Maybe a)
fromEnv f name = fmap (fmap f) (getEnv name)

fromIni :: (Text -> a) -> Text -> Text -> Ini -> Maybe a
fromIni f section key = either (const Nothing) (Just . f) . Ini.lookupValue section key

-- | A configurator mappends a monadic action producing a PreConfig to the given PreConfig.
--
-- The action may also depend on the given PreConfig.
type Configurator m = PreConfig -> m PreConfig

configureFromEnv :: (MonadConfigure m) => Configurator m
configureFromEnv a = mappend a <$> mb
  where
    mb = do
      preConfigDir <- First <$> getEnv envConfigDir
      preConfigDataDir <- First <$> getEnv envDataDir
      preConfigBackend <- First <$> fromEnv (mkBackend . T.pack) envBackend
      preConfigKeyId <- First <$> fromEnv (MkKeyId . T.pack) envKeyId
      preConfigMultKeys <- First <$> fromEnv (mkBool . T.pack) envMultKeys
      return
        MkPreConfig
          { preConfigDir,
            preConfigDataDir,
            preConfigBackend,
            preConfigKeyId,
            preConfigMultKeys
          }

configureConfigDir :: (MonadConfigure m, MonadFilesystem m) => Configurator m
configureConfigDir a = mappend a <$> mb
  where
    mb = do
      homeDir <- getHomeDir
      configDir <- getConfigDir
      let homeAnankeDir = homeDir </> dotAnankeDir
          configAnankeDir = configDir </> anankeDir
      preConfigDir <- First . Just . bool homeAnankeDir configAnankeDir <$> doesDirExist configDir
      return mempty {preConfigDir}

configureFromFile :: (MonadAppError m, MonadConfigure m, MonadFilesystem m) => Configurator m
configureFromFile a = mappend a <$> mb a
  where
    mb preConfig = do
      configDir <- maybe (throwConfiguration errNoConfigDir) return . getFirst $ preConfigDir preConfig
      configText <- readFileText $ configDir </> anankeIniFile
      configIni <- either throwConfiguration return $ Ini.parseIni configText
      let preConfigDataDir = First $ fromIni T.unpack iniSectionData iniKeyDataDir configIni
          preConfigBackend = First $ fromIni mkBackend iniSectionData iniKeyBackend configIni
          preConfigKeyId = First $ fromIni MkKeyId iniSectionGPG iniKeyKeyId configIni
          preConfigMultKeys = First $ fromIni mkBool iniSectionData iniKeyMultKeys configIni
      return mempty {preConfigDataDir, preConfigBackend, preConfigKeyId, preConfigMultKeys}

configureFromDefaults :: (MonadConfigure m, MonadFilesystem m) => Configurator m
configureFromDefaults a = mappend a <$> mb
  where
    mb = do
      homeDir <- getHomeDir
      dataDir <- getDataDir
      let homeAnankeDir = homeDir </> dotAnankeDir
          dataAnankeDir = dataDir </> anankeDir
      preConfigDataDir <- First . Just . bool homeAnankeDir dataAnankeDir <$> doesDirExist dataDir
      let preConfigMultKeys = First . Just $ False
      return mempty {preConfigDataDir, preConfigMultKeys}

mkConfig :: (MonadAppError m) => PreConfig -> m Config
mkConfig preConfig =
  MkConfig
    <$> firstOrError errConfigDir (preConfigDir preConfig)
    <*> firstOrError errDataDir (preConfigDataDir preConfig)
    <*> firstOrError errBackend (preConfigBackend preConfig)
    <*> firstOrError errKeyId (preConfigKeyId preConfig)
    <*> firstOrError errMultKeys (preConfigMultKeys preConfig)
  where
    firstOrError msg = maybe (throwConfiguration msg) pure . getFirst

configureWith ::
  forall m.
  (MonadAppError m, MonadConfigure m, MonadFilesystem m) =>
  PreConfig ->
  m Config
configureWith overrides = foldM f overrides configurators >>= mkConfig
  where
    configurators :: [Configurator m]
    configurators =
      [ configureFromEnv,
        configureConfigDir,
        configureFromFile,
        configureFromDefaults
      ]

    f :: PreConfig -> Configurator m -> m PreConfig
    f = flip ($)

configure :: (MonadAppError m, MonadConfigure m, MonadFilesystem m) => m Config
configure = configureWith mempty
