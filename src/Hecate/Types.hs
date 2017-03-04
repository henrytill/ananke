{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hecate.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

newtype ByteString64 = ByteString64 { unByteString64 :: BS.ByteString }
  deriving (Eq, Ord, Generic)

toBase64 :: BS.ByteString -> T.Text
toBase64 = decodeUtf8 . Base64.encode

instance Show ByteString64 where
  show (ByteString64 bs) = T.unpack (toBase64 bs)

instance ToField ByteString64 where
  toField (ByteString64 bs) =
    toField (toBase64 bs)

instance FromField ByteString64 where
  fromField f =
    fromField f >>= either fail (pure . ByteString64) . Base64.decode . encodeUtf8

-- | A 'Fingerprint' represents a GPG Fingerprint
newtype Fingerprint = Fingerprint T.Text
  deriving Eq

instance Show Fingerprint where
  show (Fingerprint a) = show a

-- | An 'AppConfig' represents values read from a configuration file
data AppConfig = AppConfig
  { appConfigFingerprint :: Fingerprint
  } deriving (Show, Eq)

-- | A 'Plaintext' represents a decrypted value
newtype Plaintext = Plaintext T.Text
  deriving Eq

instance Show Plaintext where
  show (Plaintext t) = show t

instance CSV.ToField Plaintext where
  toField (Plaintext bs) = CSV.toField bs

instance CSV.FromField Plaintext where
  parseField f = Plaintext <$> CSV.parseField f

-- | A 'Ciphertext' represents an encrypted value
newtype Ciphertext = Ciphertext ByteString64
  deriving (Show, Eq)

-- instance Show Ciphertext where
--   show (Ciphertext t) = show t

instance ToField Ciphertext where
  toField (Ciphertext bs) = toField bs

instance FromField Ciphertext where
  fromField f = Ciphertext <$> fromField f

-- | A 'Id' identifies a given 'Entry'.
newtype Id = Id T.Text
  deriving Eq

instance Show Id where
  show (Id d) = show d

instance ToField Id where
  toField (Id bs) = toField bs

instance FromField Id where
  fromField f = Id <$> fromField f

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = Description T.Text
  deriving Eq

instance Show Description where
  show (Description d) = show d

instance ToField Description where
  toField (Description bs) = toField bs

instance FromField Description where
  fromField f = Description <$> fromField f

instance CSV.ToField Description where
  toField (Description bs) = CSV.toField bs

instance CSV.FromField Description where
  parseField f = Description <$> CSV.parseField f

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = Identity T.Text
  deriving Eq

instance Show Identity where
  show (Identity i) = show i

instance ToField Identity where
  toField (Identity bs) = toField bs

instance FromField Identity where
  fromField f = Identity <$> fromField f

instance CSV.ToField Identity where
  toField (Identity bs) = CSV.toField bs

instance CSV.FromField Identity where
  parseField f = Identity <$> CSV.parseField f

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = Metadata T.Text
  deriving Eq

instance Show Metadata where
  show (Metadata m) = show m

instance ToField Metadata where
  toField (Metadata bs) = toField bs

instance FromField Metadata where
  fromField f = Metadata <$> fromField f

instance CSV.ToField Metadata where
  toField (Metadata bs) = CSV.toField bs

instance CSV.FromField Metadata where
  parseField f = Metadata <$> CSV.parseField f

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = Entry
  { entryId          :: Id
  , entryTimestamp   :: UTCTime
  , entryDescription :: Description
  , entryIdentity    :: Maybe Identity
  , entryCiphertext  :: Ciphertext
  , entryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

instance SQLite.FromRow Entry where
  fromRow = Entry <$> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field

instance SQLite.ToRow Entry where
  toRow Entry{..} =
    SQLite.toRow (entryId, entryTimestamp, entryDescription, entryIdentity, entryCiphertext, entryMeta)

-- | An 'InputEntry' is a record that is imported or exported from a CSV file
data InputEntry = InputEntry
  { inputDescription :: Description
  , inputIdentity    :: Maybe Identity
  , inputPlaintext   :: Plaintext
  , inputMeta        :: Maybe Metadata
  } deriving (Generic, Show, Eq)

instance CSV.FromRecord InputEntry
instance CSV.ToRecord InputEntry

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = DisplayEntry
  { displayTimestamp   :: UTCTime
  , displayDescription :: Description
  , displayIdentity    :: Maybe Identity
  , displayPlaintext   :: Plaintext
  , displayMeta        :: Maybe Metadata
  } deriving (Show, Eq)


-- | A 'Query' represents a database query
data Query = Query
  { queryId          :: Maybe Id
  , queryDescription :: Maybe Description
  , queryIdentity    :: Maybe Identity
  , queryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

newtype Ok = Ok { msg :: String }
  deriving Show

-- | 'AppError' represents application errors
data AppError
  = AuthVerification String
  | CsvDecoding String
  | GPG String
  | FileSystem String
  | Default String
  deriving (Show, Eq)

-- | 'AppContext' represents the shared environment for computations which occur
-- within an 'AppM'
data AppContext = AppContext
  { _fingerprint :: Fingerprint
  , _conn        :: SQLite.Connection
  }

-- | 'AppM' is the application monad
newtype AppM a = AppM { unAppM :: ReaderT AppContext (ExceptT AppError IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError AppError
           , MonadIO
           , MonadReader AppContext
           )

runAppM :: AppContext -> AppM a -> IO (Either AppError a)
runAppM ctx = runExceptT . flip runReaderT ctx . unAppM

-- | 'Command' represents CLI commands
data Command
  = Add { addDescription :: String
        , addIdentity    :: Maybe String
        , addMeta        :: Maybe String
        }
  | Remove { removeDescription :: String }
  | Lookup { lookupDescription :: String }
  | Import { importFile :: FilePath }
  deriving Show

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry
  | MultipleEntries [DisplayEntry]
  | Added
  | Removed
  deriving (Show, Eq)
