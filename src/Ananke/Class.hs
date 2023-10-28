module Ananke.Class
  ( MonadAppError (..),
    MonadConfigReader (..),
    MonadConfigure (..),
    MonadEncrypt (..),
    MonadFilesystem (..),
    MonadInteraction (..),
    MonadStore (..),
    MonadTime (..),
  )
where

import Ananke.Data
import Ananke.Error (AppError (..))
import Ananke.GPG qualified as GPG
import Control.Exception (throwIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import System.Directory (XdgDirectory (..))
import System.Directory qualified as Directory
import System.Environment qualified as Env
import System.IO qualified as IO

-- * MonadAppError

class (Monad m) => MonadAppError m where
  throwConfiguration :: String -> m a
  throwGPG :: String -> m a
  throwDatabase :: String -> m a
  throwFilesystem :: String -> m a
  throwAmbiguousInput :: String -> m a
  throwMigration :: String -> m a
  throwDefault :: String -> m a

instance MonadAppError IO where
  throwConfiguration = throwIO . Configuration
  throwGPG = throwIO . GPG
  throwDatabase = throwIO . Database
  throwFilesystem = throwIO . Filesystem
  throwAmbiguousInput = throwIO . AmbiguousInput
  throwMigration = throwIO . Migration
  throwDefault = throwIO . Default

instance (MonadAppError m) => MonadAppError (ReaderT r m) where
  throwConfiguration = lift . throwConfiguration
  throwGPG = lift . throwGPG
  throwDatabase = lift . throwDatabase
  throwFilesystem = lift . throwFilesystem
  throwAmbiguousInput = lift . throwAmbiguousInput
  throwMigration = lift . throwMigration
  throwDefault = lift . throwDefault

instance (MonadAppError m) => MonadAppError (StateT s m) where
  throwConfiguration = lift . throwConfiguration
  throwGPG = lift . throwGPG
  throwDatabase = lift . throwDatabase
  throwFilesystem = lift . throwFilesystem
  throwAmbiguousInput = lift . throwAmbiguousInput
  throwMigration = lift . throwMigration
  throwDefault = lift . throwDefault

-- * MonadConfigReader

class (Monad m) => MonadConfigReader m where
  askConfig :: m Config

-- * MonadConfigure

class (Monad m) => MonadConfigure m where
  getHomeDir :: m FilePath
  getConfigDir :: m FilePath
  getDataDir :: m FilePath
  getEnv :: String -> m (Maybe String)

instance MonadConfigure IO where
  getHomeDir = Directory.getHomeDirectory
  getConfigDir = Directory.getXdgDirectory XdgConfig mempty
  getDataDir = Directory.getXdgDirectory XdgData mempty
  getEnv = Env.lookupEnv

-- * MonadEncrypt

class (Monad m) => MonadEncrypt m where
  encrypt :: KeyId -> Plaintext -> m Ciphertext
  decrypt :: Ciphertext -> m Plaintext

instance MonadEncrypt IO where
  encrypt = GPG.encrypt
  decrypt = GPG.decrypt

instance (MonadEncrypt m) => MonadEncrypt (ReaderT r m) where
  encrypt k = lift . encrypt k
  decrypt = lift . decrypt

instance (MonadEncrypt m) => MonadEncrypt (StateT s m) where
  encrypt k = lift . encrypt k
  decrypt = lift . decrypt

-- * MonadFilesystem

class (Monad m) => MonadFilesystem m where
  doesFileExist :: FilePath -> m Bool
  doesDirExist :: FilePath -> m Bool
  createDir :: FilePath -> m ()
  readFileText :: FilePath -> m Text
  readFileBytes :: FilePath -> m BSL.ByteString
  writeFileBytes :: FilePath -> BSL.ByteString -> m ()

instance MonadFilesystem IO where
  doesFileExist = Directory.doesFileExist
  doesDirExist = Directory.doesDirectoryExist
  createDir = Directory.createDirectory
  readFileText = TIO.readFile
  readFileBytes = BSL.readFile
  writeFileBytes = BSL.writeFile

instance (MonadFilesystem m) => MonadFilesystem (ReaderT r m) where
  doesFileExist = lift . doesFileExist
  doesDirExist = lift . doesDirExist
  createDir = lift . createDir
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileBytes f = lift . writeFileBytes f

instance (MonadFilesystem m) => MonadFilesystem (StateT s m) where
  doesFileExist = lift . doesFileExist
  doesDirExist = lift . doesDirExist
  createDir = lift . createDir
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileBytes f = lift . writeFileBytes f

-- * MonadInteraction

class (Monad m) => MonadInteraction m where
  message :: String -> m ()
  prompt :: String -> m String

instance MonadInteraction IO where
  message = putStrLn
  prompt s = putStr s >> IO.hFlush IO.stdout >> getLine

instance (MonadInteraction m) => MonadInteraction (ReaderT r m) where
  message = lift . message
  prompt = lift . prompt

instance (MonadInteraction m) => MonadInteraction (StateT s m) where
  message = lift . message
  prompt = lift . prompt

-- * MonadStore

class (Monad m) => MonadStore m where
  put :: Entry -> m ()
  delete :: Entry -> m ()
  runQuery :: Query -> m [Entry]
  selectAll :: m [Entry]
  getCount :: m Int
  getCountOf :: KeyId -> m Int

-- * MonadTime

class (Monad m) => MonadTime m where
  now :: m UTCTime

instance MonadTime IO where
  now = Clock.getCurrentTime

instance (MonadTime m) => MonadTime (ReaderT r m) where
  now = lift now

instance (MonadTime m) => MonadTime (StateT s m) where
  now = lift now
