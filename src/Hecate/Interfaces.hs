{-# OPTIONS -Wno-unused-top-binds #-}

module Hecate.Interfaces
  ( MonadConfigReader(..)
  , MonadStore(..)
  , MonadEncrypt(..)
  , MonadInteraction(..)
  , MonadAppError(..)
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.Reader   (ReaderT)
import           Control.Monad.State    (StateT)
import           Control.Monad.Trans    (lift)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (UTCTime)
import qualified Data.Time.Clock        as Clock
import qualified Database.SQLite.Simple as SQLite
import qualified System.Directory       as Directory
import qualified System.Environment     as Env
import qualified System.IO              as IO
import           TOML                   (TOMLError)

import           Hecate.Data
import           Hecate.Error           (AppError (..))
import qualified Hecate.GPG             as GPG


-- * MonadConfigReaer

class Monad m => MonadConfigReader m where
  askConfig :: m Config

-- * MonadStore

class Monad m => MonadStore m where
  put                  :: Entry         -> m ()
  delete               :: Entry         -> m ()
  query                :: Query         -> m [Entry]
  selectAll            :: m [Entry]
  getCount             :: m Int
  getCountOfKeyId      :: KeyId         -> m Int
  createTable          :: m ()
  migrate              :: SchemaVersion -> KeyId -> m ()
  currentSchemaVersion :: m SchemaVersion

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

instance MonadEncrypt m => MonadEncrypt (StateT s m) where
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
  now                         = Clock.getCurrentTime
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
  prompt s                    = putStr s >> IO.hFlush IO.stdout >> getLine

instance MonadInteraction m => MonadInteraction (ReaderT r m)  where
  now                            = lift now
  doesFileExist                  = lift . doesFileExist
  doesDirectoryExist             = lift . doesDirectoryExist
  createDirectory                = lift . createDirectory
  openSQLiteFile                 = lift . openSQLiteFile
  closeSQLiteConnection          = lift . closeSQLiteConnection
  readFileAsString               = lift . readFileAsString
  readFileAsText                 = lift . readFileAsText
  readFileAsLazyByteString       = lift . readFileAsLazyByteString
  writeFileFromString fp         = lift . writeFileFromString fp
  writeFileFromLazyByteString fp = lift . writeFileFromLazyByteString fp
  getEnv                         = lift . getEnv
  message                        = lift . message
  prompt                         = lift . prompt

instance MonadInteraction m => MonadInteraction (StateT s m)  where
  now                            = lift now
  doesFileExist                  = lift . doesFileExist
  doesDirectoryExist             = lift . doesDirectoryExist
  createDirectory                = lift . createDirectory
  openSQLiteFile                 = lift . openSQLiteFile
  closeSQLiteConnection          = lift . closeSQLiteConnection
  readFileAsString               = lift . readFileAsString
  readFileAsText                 = lift . readFileAsText
  readFileAsLazyByteString       = lift . readFileAsLazyByteString
  writeFileFromString fp         = lift . writeFileFromString fp
  writeFileFromLazyByteString fp = lift . writeFileFromLazyByteString fp
  getEnv                         = lift . getEnv
  message                        = lift . message
  prompt                         = lift . prompt

-- * MonadAppError

class Monad m => MonadAppError m where
  csvDecodingError    :: String    -> m a
  tomlError           :: TOMLError -> m a
  configurationError  :: String    -> m a
  aesonError          :: String    -> m a
  gpgError            :: String    -> m a
  databaseError       :: String    -> m a
  fileSystemError     :: String    -> m a
  ambiguousInputError :: String    -> m a
  migrationError      :: String    -> m a
  defaultError        :: String    -> m a

instance MonadAppError IO where
  csvDecodingError    = throwM . CsvDecoding
  tomlError           = throwM . TOML
  configurationError  = throwM . Configuration
  aesonError          = throwM . Aeson
  gpgError            = throwM . GPG
  databaseError       = throwM . Database
  fileSystemError     = throwM . FileSystem
  ambiguousInputError = throwM . AmbiguousInput
  migrationError      = throwM . Migration
  defaultError        = throwM . Default

instance MonadAppError m => MonadAppError (ReaderT r m) where
  csvDecodingError    = lift . csvDecodingError
  tomlError           = lift . tomlError
  configurationError  = lift . configurationError
  aesonError          = lift . aesonError
  gpgError            = lift . gpgError
  databaseError       = lift . databaseError
  fileSystemError     = lift . fileSystemError
  ambiguousInputError = lift . ambiguousInputError
  migrationError      = lift . migrationError
  defaultError        = lift . defaultError

instance MonadAppError m => MonadAppError (StateT s m) where
  csvDecodingError    = lift . csvDecodingError
  tomlError           = lift . tomlError
  configurationError  = lift . configurationError
  aesonError          = lift . aesonError
  gpgError            = lift . gpgError
  databaseError       = lift . databaseError
  fileSystemError     = lift . fileSystemError
  ambiguousInputError = lift . ambiguousInputError
  migrationError      = lift . migrationError
  defaultError        = lift . defaultError
