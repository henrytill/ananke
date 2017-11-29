{-# LANGUAGE DeriveGeneric #-}

module Hecate.Data
  ( -- * Configuration
    PreConfig(..)
  , Config(..)
  , AppContext(..)
    -- * Import & Display Entries
  , CSVEntry
  , _csvDescription
  , _csvIdentity
  , _csvPlaintext
  , _csvMeta
  , entryToCSVEntry
  , DisplayEntry(..)
  , entryToDisplayEntry
    -- * Entries
  , Entry
  , _entryId
  , _entryDescription
  , _entryIdentity
  , _entryCiphertext
  , _entryMeta
  , createEntry
    -- ** Their constituents
  , Id
  , unId
  , Description(..)
  , Identity(..)
  , Metadata(..)
    -- ** And some updaters
  , updateKeyId
  , updateDescription
  , updateIdentity
  , updateMetadata
    -- * Queries
  , Query
  , _queryId
  , _queryIdentity
  , _queryDescription
  , _queryMeta
  , query
  , queryIsEmpty
  -- * Count
  , Count
  , unCount
  ) where

import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Csv                         as CSV
import           Data.Digest.Pure.SHA             (sha1, showDigest)
import           Data.Monoid                      (First, (<>))
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Format                 (defaultTimeLocale,
                                                   formatTime)
import qualified Database.SQLite.Simple           as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics

import           Hecate.GPG                       (Ciphertext, KeyId (..),
                                                   Plaintext)

-- * Configuration

-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = PreConfig
  { _preConfigDataDirectory     :: First FilePath
  , _preConfigKeyId             :: First KeyId
  , _preConfigAllowMultipleKeys :: First Bool
  } deriving (Show, Eq)

instance Monoid PreConfig where
  mempty
    = PreConfig mempty mempty mempty

  PreConfig a b c `mappend` PreConfig d e f
    = PreConfig (a `mappend` d)
                (b `mappend` e)
                (c `mappend` f)

-- | A 'Config' represents our application's configuration
data Config = Config
  { _configDataDirectory     :: FilePath
  , _configKeyId             :: KeyId
  , _configAllowMultipleKeys :: Bool
  } deriving (Show, Eq)

-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { _appContextConfig     :: Config
  , _appContextConnection :: SQLite.Connection
  }

-- * Import and Display Entries

-- | An 'CSVEntry' is a record that is imported or exported from a CSV file
data CSVEntry = CSVEntry
  { _csvDescription :: Description
  , _csvIdentity    :: Maybe Identity
  , _csvPlaintext   :: Plaintext
  , _csvMeta        :: Maybe Metadata
  } deriving (Generic, Show, Eq)

instance CSV.FromRecord CSVEntry
instance CSV.ToRecord CSVEntry

entryToCSVEntry
  :: Monad m
  => (Ciphertext -> m Plaintext)
  -> Entry
  -> m CSVEntry
entryToCSVEntry decrypt e
  = f e <$> decrypt (_entryCiphertext e)
  where
    f entry plaintext = CSVEntry (_entryDescription entry)
                                 (_entryIdentity entry)
                                 plaintext
                                 (_entryMeta entry)

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = DisplayEntry
  { _displayId          :: Id
  , _displayTimestamp   :: UTCTime
  , _displayDescription :: Description
  , _displayIdentity    :: Maybe Identity
  , _displayPlaintext   :: Plaintext
  , _displayMeta        :: Maybe Metadata
  } deriving (Show, Eq)

entryToDisplayEntry
  :: Monad m
  => (Ciphertext -> m Plaintext)
  -> Entry
  -> m DisplayEntry
entryToDisplayEntry decrypt e
  = f e <$> decrypt (_entryCiphertext e)
  where
    f entry plaintext = DisplayEntry (_entryId entry)
                                     (_entryTimestamp entry)
                                     (_entryDescription entry)
                                     (_entryIdentity entry)
                                     plaintext
                                     (_entryMeta entry)

-- * Entries

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = Entry
  { _entryId          :: Id
  , _entryKeyId       :: KeyId
  , _entryTimestamp   :: UTCTime
  , _entryDescription :: Description
  , _entryIdentity    :: Maybe Identity
  , _entryCiphertext  :: Ciphertext
  , _entryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

instance SQLite.FromRow Entry where
  fromRow = Entry <$> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field

instance SQLite.ToRow Entry where
  toRow entry = SQLite.toRow ( _entryId          entry
                             , _entryKeyId       entry
                             , _entryTimestamp   entry
                             , _entryDescription entry
                             , _entryIdentity    entry
                             , _entryCiphertext  entry
                             , _entryMeta        entry
                             )

showTime :: UTCTime -> T.Text
showTime = T.pack . formatTime defaultTimeLocale "%s%Q"

ider :: T.Text -> Id
ider = Id . T.pack . showDigest . sha1 . BSL.fromStrict . encodeUtf8

createId :: KeyId -> UTCTime -> Description -> Maybe Identity -> Id
createId (KeyId k) ts (Description d) (Just (Identity i)) =
  ider (k <> showTime ts <> d <> i)
createId (KeyId k) ts (Description d) Nothing =
  ider (k <> showTime ts <> d)

createEntry
  :: Monad m
  => (KeyId -> Plaintext -> m Ciphertext)
  -> KeyId
  -> UTCTime
  -> Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
createEntry encrypt keyId timestamp description identity plaintext meta = do
  i         <- pure (createId keyId timestamp description identity)
  encrypted <- encrypt keyId plaintext
  return (Entry i keyId timestamp description identity encrypted meta)

updateEntry
  :: Monad m
  => KeyId
  -> UTCTime
  -> Description
  -> Maybe Identity
  -> Ciphertext
  -> Maybe Metadata
  -> m Entry
updateEntry keyId timestamp description identity ciphertext meta = do
  i         <- pure (createId keyId timestamp description identity)
  return (Entry i keyId timestamp description identity ciphertext meta)

-- ** Their constituents

-- | A 'Id' identifies a given 'Entry'.
newtype Id = Id { unId :: T.Text }
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

-- ** And some updaters

updateKeyId
  :: Monad m
  => (Ciphertext -> m Plaintext)
  -> (KeyId -> Plaintext -> m Ciphertext)
  -> KeyId
  -> Entry
  -> m Entry
updateKeyId decrypt encrypt keyId entry = do
  plaintext <- decrypt (_entryCiphertext entry)
  createEntry encrypt
              keyId
              (_entryTimestamp entry)
              (_entryDescription entry)
              (_entryIdentity entry)
              plaintext
              (_entryMeta entry)

updateDescription
  :: Monad m
  => UTCTime
  -> Description
  -> Entry
  -> m Entry
updateDescription now desc entry
  = updateEntry (_entryKeyId entry)
                now
                desc
                (_entryIdentity entry)
                (_entryCiphertext entry)
                (_entryMeta entry)

updateIdentity
  :: Monad m
  => UTCTime
  -> Maybe Identity
  -> Entry
  -> m Entry
updateIdentity now iden entry
  = updateEntry (_entryKeyId entry)
                now
                (_entryDescription entry)
                iden
                (_entryCiphertext entry)
                (_entryMeta entry)

updateMetadata
  :: Monad m
  => UTCTime
  -> Maybe Metadata
  -> Entry
  -> m Entry
updateMetadata now meta entry
  = updateEntry (_entryKeyId entry)
                now
                (_entryDescription entry)
                (_entryIdentity entry)
                (_entryCiphertext entry)
                meta

-- * Queries

-- | A 'Query' represents a database query
data Query = Query
  { _queryId          :: Maybe Id
  , _queryDescription :: Maybe Description
  , _queryIdentity    :: Maybe Identity
  , _queryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

query :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Query
query i d iden m =
  Query { _queryId          = (Id          . T.pack) <$> i
        , _queryDescription = (Description . T.pack) <$> d
        , _queryIdentity    = (Identity    . T.pack) <$> iden
        , _queryMeta        = (Metadata    . T.pack) <$> m
        }

queryIsEmpty :: Query -> Bool
queryIsEmpty (Query Nothing Nothing Nothing Nothing) = True
queryIsEmpty _                                       = False

-- * Count

-- | A 'Count' represents the results of a count query
newtype Count = Count { unCount :: Int }
  deriving (Show, Eq)

instance SQLite.FromRow Count where
  fromRow = Count <$> SQLite.field
