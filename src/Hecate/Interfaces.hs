module Hecate.Interfaces
  ( HasConfig(..)
  , HasAppContext(..)
  , MonadInteraction(..)
  , MonadStore(..)
  , MonadEncrypt(..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..), ReaderT)
import           Control.Monad.Trans    (lift)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2
import           Lens.Family2.Unchecked (lens)
import qualified System.Directory       as Directory
import           System.IO              (hFlush, stdout)
import qualified System.Environment     as Env

import           Hecate.Data
import           Hecate.Database        (SchemaVersion)
import qualified Hecate.Database        as DB
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

withConnection
  :: (MonadReader r m, HasAppContext r)
  => (SQLite.Connection -> m a)
  -> m a
withConnection f = ask >>= \ ctx -> f (ctx ^. appContextConnection)

instance (MonadThrow m, MonadIO m, HasAppContext r) => MonadStore (ReaderT r m) where
  put             e       = withConnection (`DB.put` e)
  delete          e       = withConnection (`DB.delete` e)
  query           q       = withConnection (`DB.query` q)
  selectAll               = withConnection DB.selectAll
  getCount                = withConnection DB.getCount
  getCountOfKeyId kid     = withConnection (`DB.getCountOfKeyId` kid)
  createTable             = withConnection DB.createTable
  migrate         sv  kid = withConnection (\ conn -> DB.migrate conn sv kid)

-- * MonadEncrypt

class Monad m => MonadEncrypt m where
  encrypt :: KeyId      -> Plaintext -> m Ciphertext
  decrypt :: Ciphertext -> m Plaintext

instance MonadEncrypt IO where
  encrypt = GPG.encrypt
  decrypt = GPG.decrypt

instance MonadEncrypt m => MonadEncrypt (ReaderT r m) where
  encrypt kid pt = lift (encrypt kid pt)
  decrypt     ct = lift (decrypt ct)

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
  getEnv                      = Env.lookupEnv
  message                     = putStrLn
  prompt s                    = putStr s >> hFlush stdout >> getLine

instance MonadInteraction m => MonadInteraction (ReaderT r m)  where
  now                              = lift now
  doesFileExist                    = lift . doesFileExist
  doesDirectoryExist               = lift . doesDirectoryExist
  createDirectory                  = lift . createDirectory
  openSQLiteFile                   = lift . openSQLiteFile
  closeSQLiteConnection            = lift . closeSQLiteConnection
  readFileAsString                 = lift . readFileAsString
  readFileAsText                   = lift . readFileAsText
  readFileAsLazyByteString         = lift . readFileAsLazyByteString
  writeFileFromString fp s         = lift (writeFileFromString fp s)
  writeFileFromLazyByteString fp s = lift (writeFileFromLazyByteString fp s)
  getEnv                           = lift . getEnv
  message                          = lift . message
  prompt                           = lift . prompt
