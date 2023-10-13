module Ananke.Class
  ( MonadAppError(..)
  , MonadConfigReader(..)
  , MonadConfigure(..)
  , MonadEncrypt(..)
  , MonadFilesystem(..)
  , MonadInteraction(..)
  , MonadStore(..)
  , MonadTime(..)
  ) where

import           Prelude              (Bool, FilePath, IO, Int, Maybe, Monad, String, ($), (.), (>>))
import qualified Prelude

import           Control.Monad.Catch  (MonadThrow (..))
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (StateT)
import           Control.Monad.Trans  (lift)
import qualified Data.ByteString.Lazy as BSL
import           Data.Time.Clock      (UTCTime)
import qualified Data.Time.Clock      as Clock
import qualified System.Directory     as Directory
import qualified System.Environment   as Env
import qualified System.IO            as IO

import           Ananke.Data
import           Ananke.Error         (AppError (..))
import qualified Ananke.GPG           as GPG


-- * MonadAppError

class Monad m => MonadAppError m where
  configurationError  :: String -> m a
  gpgError            :: String -> m a
  databaseError       :: String -> m a
  filesystemError     :: String -> m a
  ambiguousInputError :: String -> m a
  migrationError      :: String -> m a
  defaultError        :: String -> m a

instance MonadAppError IO where
  configurationError  = throwM . Configuration
  gpgError            = throwM . GPG
  databaseError       = throwM . Database
  filesystemError     = throwM . Filesystem
  ambiguousInputError = throwM . AmbiguousInput
  migrationError      = throwM . Migration
  defaultError        = throwM . Default

instance MonadAppError m => MonadAppError (ReaderT r m) where
  configurationError  = lift . configurationError
  gpgError            = lift . gpgError
  databaseError       = lift . databaseError
  filesystemError     = lift . filesystemError
  ambiguousInputError = lift . ambiguousInputError
  migrationError      = lift . migrationError
  defaultError        = lift . defaultError

instance MonadAppError m => MonadAppError (StateT s m) where
  configurationError  = lift . configurationError
  gpgError            = lift . gpgError
  databaseError       = lift . databaseError
  filesystemError     = lift . filesystemError
  ambiguousInputError = lift . ambiguousInputError
  migrationError      = lift . migrationError
  defaultError        = lift . defaultError

-- * MonadConfigReader

class Monad m => MonadConfigReader m where
  askConfig :: m Config

-- * MonadConfigure

class Monad m => MonadConfigure m where
  readConfigFile :: FilePath -> m String
  getEnv         :: String   -> m (Maybe String)

instance MonadConfigure IO where
  readConfigFile = Prelude.readFile
  getEnv         = Env.lookupEnv

-- * MonadEncrypt

class Monad m => MonadEncrypt m where
  encrypt :: KeyId      -> Plaintext -> m Ciphertext
  decrypt :: Ciphertext -> m Plaintext

instance MonadEncrypt IO where
  encrypt = GPG.encrypt
  decrypt = GPG.decrypt

instance MonadEncrypt m => MonadEncrypt (ReaderT r m) where
  encrypt k p = lift $ encrypt k p
  decrypt   c = lift $ decrypt c

instance MonadEncrypt m => MonadEncrypt (StateT s m) where
  encrypt k p = lift $ encrypt k p
  decrypt   c = lift $ decrypt c

-- * MonadFilesystem

class Monad m => MonadFilesystem m where
  doesFileExist      :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  createDirectory    :: FilePath -> m ()
  readFileBytes      :: FilePath -> m BSL.ByteString
  writeFileBytes     :: FilePath -> BSL.ByteString -> m ()

instance MonadFilesystem IO where
  doesFileExist      = Directory.doesFileExist
  doesDirectoryExist = Directory.doesDirectoryExist
  createDirectory    = Directory.createDirectory
  readFileBytes      = BSL.readFile
  writeFileBytes     = BSL.writeFile

instance MonadFilesystem m => MonadFilesystem (ReaderT r m) where
  doesFileExist      = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  createDirectory    = lift . createDirectory
  readFileBytes      = lift . readFileBytes
  writeFileBytes f   = lift . writeFileBytes f

instance MonadFilesystem m => MonadFilesystem (StateT s m) where
  doesFileExist      = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  createDirectory    = lift . createDirectory
  readFileBytes      = lift . readFileBytes
  writeFileBytes f   = lift . writeFileBytes f

-- * MonadInteraction

class Monad m => MonadInteraction m where
  message :: String -> m ()
  prompt  :: String -> m String

instance MonadInteraction IO where
  message  = Prelude.putStrLn
  prompt s = Prelude.putStr s >> IO.hFlush IO.stdout >> Prelude.getLine

instance MonadInteraction m => MonadInteraction (ReaderT r m) where
  message = lift . message
  prompt  = lift . prompt

instance MonadInteraction m => MonadInteraction (StateT s m) where
  message = lift . message
  prompt  = lift . prompt

-- * MonadStore

class Monad m => MonadStore m where
  put             :: Entry -> m ()
  delete          :: Entry -> m ()
  runQuery        :: Query -> m [Entry]
  selectAll       :: m [Entry]
  getCount        :: m Int
  getCountOfKeyId :: KeyId -> m Int

-- * MonadTime

class Monad m => MonadTime m where
  now :: m UTCTime

instance MonadTime IO where
  now = Clock.getCurrentTime

instance MonadTime m => MonadTime (ReaderT r m) where
  now = lift now

instance MonadTime m => MonadTime (StateT s m) where
  now = lift now
