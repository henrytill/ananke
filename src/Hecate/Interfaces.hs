module Hecate.Interfaces
  ( MonadInteraction(..)
  , MonadStore(..)
  , MonadEncrypt(..)
  ) where

import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import qualified Database.SQLite.Simple as SQLite
import qualified System.Directory       as Directory
import           System.IO              (hFlush, stdout)
import qualified System.Posix.Env       as Env

import           Hecate.Data            (Entry, Query)
import           Hecate.Database        (SchemaVersion)
import           Hecate.GPG             (Ciphertext, KeyId, Plaintext)
import qualified Hecate.GPG             as GPG

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
