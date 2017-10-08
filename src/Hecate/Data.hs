{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Hecate.Data
  ( -- * Import & Display Entries
    CSVEntry
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
  , csvEntryToEntry
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
  , updateCiphertext
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

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Csv                         as CSV
import           Data.Digest.Pure.SHA             (sha1, showDigest)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock                  (UTCTime, getCurrentTime)
import           Data.Time.Format                 (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple           as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics
import           Lens.Family2

import           Hecate.Context
import           Hecate.GPG


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
  :: MonadIO m
  => Entry
  -> m CSVEntry
entryToCSVEntry e = f e <$> decrypt (_entryCiphertext e)
  where
    f Entry{_entryDescription, _entryIdentity, _entryMeta} p
      = CSVEntry _entryDescription _entryIdentity p _entryMeta

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
  :: MonadIO m
  => Entry
  -> m DisplayEntry
entryToDisplayEntry e = f e <$> decrypt (_entryCiphertext e)
  where
    f Entry{_entryId, _entryTimestamp, _entryDescription, _entryIdentity, _entryMeta} p
      = DisplayEntry _entryId _entryTimestamp _entryDescription _entryIdentity p _entryMeta


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
  toRow Entry{_entryId, _entryKeyId, _entryTimestamp, _entryDescription, _entryIdentity, _entryCiphertext, _entryMeta} =
    SQLite.toRow ( _entryId
                 , _entryKeyId
                 , _entryTimestamp
                 , _entryDescription
                 , _entryIdentity
                 , _entryCiphertext
                 , _entryMeta
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

createEntryImpl
  :: MonadIO m
  => KeyId
  -> UTCTime
  -> Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
createEntryImpl keyId timestamp description identity plaintext meta = do
  i         <- pure (createId keyId timestamp description identity)
  encrypted <- encrypt keyId plaintext
  return (Entry i keyId timestamp description identity encrypted meta)

createEntry
  :: (MonadIO m, MonadReader r m, HasConfig r)
  => Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
createEntry description identity plaintext meta = do
  ctx       <- ask
  timestamp <- liftIO getCurrentTime
  createEntryImpl (ctx ^. configKeyId) timestamp description identity plaintext meta

updateEntry
  :: MonadIO m
  => KeyId
  -> Description
  -> Maybe Identity
  -> Ciphertext
  -> Maybe Metadata
  -> m Entry
updateEntry keyId description identity ciphertext meta = do
  timestamp <- liftIO getCurrentTime
  i         <- pure (createId keyId timestamp description identity)
  return (Entry i keyId timestamp description identity ciphertext meta)

csvEntryToEntry
  :: (MonadIO m, MonadReader r m, HasConfig r)
  => CSVEntry
  -> m Entry
csvEntryToEntry CSVEntry{_csvDescription, _csvIdentity, _csvPlaintext, _csvMeta} =
  createEntry _csvDescription _csvIdentity _csvPlaintext _csvMeta


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
  :: MonadIO m
  => KeyId
  -> Entry
  -> m Entry
updateKeyId keyId entry@Entry{_entryTimestamp, _entryDescription, _entryIdentity, _entryMeta} =
  decrypt (_entryCiphertext entry) >>= \ pt ->
  createEntryImpl keyId _entryTimestamp _entryDescription _entryIdentity pt _entryMeta

updateDescription
  :: MonadIO m
  => Description
  -> Entry
  -> m Entry
updateDescription d Entry{_entryKeyId, _entryIdentity, _entryCiphertext, _entryMeta} =
  updateEntry _entryKeyId d _entryIdentity _entryCiphertext _entryMeta

updateIdentity
  :: MonadIO m
  => Maybe Identity
  -> Entry
  -> m Entry
updateIdentity iden Entry{_entryKeyId, _entryDescription, _entryCiphertext, _entryMeta} =
  updateEntry _entryKeyId _entryDescription iden _entryCiphertext _entryMeta

updateCiphertext
  :: (MonadIO m, MonadReader r m, HasConfig r)
  => Plaintext
  -> Entry
  -> m Entry
updateCiphertext pt Entry{_entryDescription, _entryIdentity, _entryMeta} =
  createEntry _entryDescription _entryIdentity pt _entryMeta

updateMetadata
  :: MonadIO m
  => Maybe Metadata
  -> Entry
  -> m Entry
updateMetadata m Entry{_entryKeyId, _entryDescription, _entryIdentity, _entryCiphertext} =
  updateEntry _entryKeyId _entryDescription _entryIdentity _entryCiphertext m


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
