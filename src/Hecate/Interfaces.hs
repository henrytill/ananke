module Hecate.Interfaces
  ( HasConfig(..)
  , HasAppContext(..)
  , MonadInteraction(..)
  , MonadStore(..)
  , MonadEncrypt(..)
  ) where

import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2
import           Lens.Family2.Unchecked (lens)
import qualified System.Directory       as Directory
import           System.IO              (hFlush, stdout)
import qualified System.Posix.Env       as Env

import           Hecate.Data
import           Hecate.Database        (SchemaVersion)
import           Hecate.GPG             (Ciphertext, KeyId, Plaintext)
import qualified Hecate.GPG             as GPG

-- * Has* Classes

class HasConfig t where
  config                  :: Lens' t Config
  configDataDirectory     :: Lens' t FilePath
  configKeyId             :: Lens' t KeyId
  configAllowMultipleKeys :: Lens' t Bool
  configDatabaseDirectory :: Getter' t FilePath
  configSchemaFile        :: Getter' t FilePath
  configDatabaseFile      :: Getter' t FilePath
  configDataDirectory     = config . configDataDirectory
  configKeyId             = config . configKeyId
  configAllowMultipleKeys = config . configAllowMultipleKeys
  configDatabaseDirectory = config . configDatabaseDirectory
  configSchemaFile        = config . configSchemaFile
  configDatabaseFile      = config . configDatabaseFile

instance HasConfig Config where
  config                  = id
  configDataDirectory     = lens _configDataDirectory     (\ c dir      -> c{_configDataDirectory     = dir})
  configKeyId             = lens _configKeyId             (\ c keyId    -> c{_configKeyId             = keyId})
  configAllowMultipleKeys = lens _configAllowMultipleKeys (\ c multKeys -> c{_configAllowMultipleKeys = multKeys})
  configDatabaseDirectory = configDataDirectory     . to (++ "/db")
  configSchemaFile        = configDatabaseDirectory . to (++ "/schema")
  configDatabaseFile      = configDatabaseDirectory . to (++ "/db.sqlite")

instance HasConfig AppContext where
  config = lens _appContextConfig (\ a cfg -> a{_appContextConfig = cfg})

class HasConfig t => HasAppContext t where
  appContext           :: Lens' t AppContext
  appContextConfig     :: Lens' t Config
  appContextConnection :: Lens' t SQLite.Connection
  appContextConfig     = appContext . appContextConfig
  appContextConnection = appContext . appContextConnection

instance HasAppContext AppContext where
  appContext           = id
  appContextConfig     = config
  appContextConnection = lens _appContextConnection (\ a conn -> a{_appContextConnection = conn})

-- * MonadStore

class Monad m => MonadStore m where
  put             :: Entry         -> m ()
  delete          :: Entry         -> m ()
  query           :: Query         -> m [Entry]
  selectAll       :: m [Entry]
  getCount        :: m Int
  getCountOfKeyId :: KeyId         -> m Int
  createTable     :: m ()
  migrate         :: SchemaVersion -> KeyId -> m ()

-- * MonadEncrypt

class Monad m => MonadEncrypt m where
  encrypt :: KeyId      -> Plaintext -> m Ciphertext
  decrypt :: Ciphertext -> m Plaintext

instance MonadEncrypt IO where
  encrypt = GPG.encrypt
  decrypt = GPG.decrypt

-- * MonadInteraction

class Monad m => MonadInteraction m where
  now                         :: m UTCTime
  doesFileExist               :: FilePath          -> m Bool
  doesDirectoryExist          :: FilePath          -> m Bool
  createDirectory             :: FilePath          -> m ()
  openSQLiteFile              :: FilePath          -> m SQLite.Connection
  closeSQLiteConnection       :: SQLite.Connection -> m ()
  readFileAsString            :: FilePath          -> m String
  readFileAsLazyByteString    :: FilePath          -> m BSL.ByteString
  readFileAsText              :: FilePath          -> m T.Text
  writeFileFromString         :: FilePath          -> String         -> m ()
  writeFileFromLazyByteString :: FilePath          -> BSL.ByteString -> m ()
  getEnv                      :: String            -> m (Maybe String)
  message                     :: String            -> m ()
  prompt                      :: String            -> m String

instance MonadInteraction IO where
  now                         = getCurrentTime
  doesFileExist               = Directory.doesFileExist
  doesDirectoryExist          = Directory.doesDirectoryExist
  createDirectory             = Directory.createDirectory
  openSQLiteFile              = SQLite.open
  closeSQLiteConnection       = SQLite.close
  readFileAsString            = readFile
  readFileAsText              = TIO.readFile
  readFileAsLazyByteString    = BSL.readFile
  writeFileFromString         = writeFile
  writeFileFromLazyByteString = BSL.writeFile
  getEnv                      = Env.getEnv
  message                     = putStrLn
  prompt s                    = putStr s >> hFlush stdout >> getLine
