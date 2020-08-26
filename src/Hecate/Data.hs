{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Data
  ( -- * Configuration
    Backend(..)
  , PreConfig(..)
  , Config(..)
  , HasConfig(..)
  , SchemaVersion(..)
    -- * Import & Display Entries
  , CSVEntry
  , _csvDescription
  , _csvIdentity
  , _csvPlaintext
  , _csvMeta
  , entryToCSVEntry
  , DisplayEntry(..)
  , entryToDisplayEntry
  , KeyId(..)
    -- * Decrypted and encrypted values
  , Plaintext(..)
  , Ciphertext
  , mkCiphertext
  , unCiphertext
    -- * Entries
  , Entry
  , _entryId
  , _entryKeyId
  , _entryDescription
  , _entryIdentity
  , _entryCiphertext
  , _entryMeta
  , entryId
  , entryKeyId
  , entryTimestamp
  , entryDescription
  , entryIdentity
  , entryCiphertext
  , entryMeta
  , entryKeyOrder
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

import           Data.Aeson                       (FromJSON (..), Options, ToJSON (..))
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.ByteString64                (ByteString64 (..))
import qualified Data.Csv                         as CSV
import qualified Data.Digest.Pure.SHA             as SHA
import qualified Data.List                        as List
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (First)
import qualified Data.Ord                         as Ord
import qualified Data.Semigroup                   as Sem
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as Encoding
import           Data.Time.Clock                  (UTCTime)
import qualified Data.Time.Format                 as Format
import qualified Database.SQLite.Simple           as SQLite
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           GHC.Generics                     (Generic)
import           Lens.Family2                     (Getter', Lens', to)
import           Lens.Family2.Unchecked           (lens)


-- * Configuration

data Backend = SQLite | JSON
  deriving (Eq, Show)

-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = PreConfig
  { _preConfigDataDirectory     :: First FilePath
  , _preConfigBackend           :: First Backend
  , _preConfigKeyId             :: First KeyId
  , _preConfigAllowMultipleKeys :: First Bool
  } deriving (Show, Eq)

instance Sem.Semigroup PreConfig where
  (<>) = mappend

instance Monoid PreConfig where
  mempty
    = PreConfig mempty mempty mempty mempty

  PreConfig a b c d `mappend` PreConfig e f g h
    = PreConfig (a `mappend` e)
                (b `mappend` f)
                (c `mappend` g)
                (d `mappend` h)

-- | A 'Config' represents our application's configuration
data Config = Config
  { _configDataDirectory     :: FilePath
  , _configBackend           :: Backend
  , _configKeyId             :: KeyId
  , _configAllowMultipleKeys :: Bool
  } deriving (Show, Eq)

class HasConfig t where
  config                  :: Lens' t Config
  configDataDirectory     :: Lens' t FilePath
  configBackend           :: Lens' t Backend
  configKeyId             :: Lens' t KeyId
  configAllowMultipleKeys :: Lens' t Bool
  configDatabaseDirectory :: Getter' t FilePath
  configSchemaFile        :: Getter' t FilePath
  configDatabaseFile      :: Getter' t FilePath
  configDataFile          :: Getter' t FilePath
  configDataDirectory     = config . configDataDirectory
  configBackend           = config . configBackend
  configKeyId             = config . configKeyId
  configAllowMultipleKeys = config . configAllowMultipleKeys
  configDatabaseDirectory = config . configDatabaseDirectory
  configSchemaFile        = config . configSchemaFile
  configDatabaseFile      = config . configDatabaseFile
  configDataFile          = config . configDataFile
  {-# INLINE configDataDirectory     #-}
  {-# INLINE configBackend           #-}
  {-# INLINE configKeyId             #-}
  {-# INLINE configAllowMultipleKeys #-}
  {-# INLINE configDatabaseDirectory #-}
  {-# INLINE configSchemaFile        #-}
  {-# INLINE configDatabaseFile      #-}
  {-# INLINE configDataFile          #-}

instance HasConfig Config where
  config                  = id
  configDataDirectory     = lens _configDataDirectory     (\ c v -> c{_configDataDirectory     = v})
  configBackend           = lens _configBackend           (\ c v -> c{_configBackend           = v})
  configKeyId             = lens _configKeyId             (\ c v -> c{_configKeyId             = v})
  configAllowMultipleKeys = lens _configAllowMultipleKeys (\ c v -> c{_configAllowMultipleKeys = v})
  configDatabaseDirectory = configDataDirectory     . to (++ "/db")
  configSchemaFile        = configDatabaseDirectory . to (++ "/schema")
  configDatabaseFile      = configDatabaseDirectory . to (++ "/db.sqlite")
  configDataFile          = configDatabaseDirectory . to (++ "/data.json")
  {-# INLINE config                  #-}
  {-# INLINE configDataDirectory     #-}
  {-# INLINE configBackend           #-}
  {-# INLINE configKeyId             #-}
  {-# INLINE configAllowMultipleKeys #-}
  {-# INLINE configDatabaseDirectory #-}
  {-# INLINE configSchemaFile        #-}
  {-# INLINE configDatabaseFile      #-}
  {-# INLINE configDataFile          #-}

-- | A 'SchemaVersion' represents the database's schema version
newtype SchemaVersion = SchemaVersion { unSchemaVersion :: Int }
  deriving Eq

instance Show SchemaVersion where
  show = show . unSchemaVersion

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

-- | A 'KeyId' represents a GPG Key Id
newtype KeyId = KeyId { unKeyId :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show KeyId where
  show (KeyId a) = show a

instance ToJSON KeyId where
  toJSON = toJSON . unKeyId

instance FromJSON KeyId where
  parseJSON = fmap KeyId . parseJSON

instance ToField KeyId where
  toField (KeyId bs) = toField bs

instance FromField KeyId where
  fromField f = KeyId <$> fromField f

-- * Decrypted and encrypted values

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
  deriving (Show, Eq, Ord, Generic)

mkCiphertext :: BS.ByteString -> Ciphertext
mkCiphertext = Ciphertext . ByteString64

unCiphertext :: Ciphertext -> BS.ByteString
unCiphertext (Ciphertext bs64) = unByteString64 bs64

instance ToJSON Ciphertext where

instance FromJSON Ciphertext where

instance ToField Ciphertext where
  toField (Ciphertext bs) = toField bs

instance FromField Ciphertext where
  fromField f = Ciphertext <$> fromField f


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
  } deriving (Show, Eq, Generic)

entryId          :: Lens' Entry Id
entryKeyId       :: Lens' Entry KeyId
entryTimestamp   :: Lens' Entry UTCTime
entryDescription :: Lens' Entry Description
entryIdentity    :: Lens' Entry (Maybe Identity)
entryCiphertext  :: Lens' Entry Ciphertext
entryMeta        :: Lens' Entry (Maybe Metadata)
entryId          = lens _entryId          (\ e v -> e{_entryId          = v})
entryKeyId       = lens _entryKeyId       (\ e v -> e{_entryKeyId       = v})
entryTimestamp   = lens _entryTimestamp   (\ e v -> e{_entryTimestamp   = v})
entryDescription = lens _entryDescription (\ e v -> e{_entryDescription = v})
entryIdentity    = lens _entryIdentity    (\ e v -> e{_entryIdentity    = v})
entryCiphertext  = lens _entryCiphertext  (\ e v -> e{_entryCiphertext  = v})
entryMeta        = lens _entryMeta        (\ e v -> e{_entryMeta        = v})
{-# INLINE entryId          #-}
{-# INLINE entryKeyId       #-}
{-# INLINE entryTimestamp   #-}
{-# INLINE entryDescription #-}
{-# INLINE entryIdentity    #-}
{-# INLINE entryCiphertext  #-}
{-# INLINE entryMeta        #-}

entryKeyOrder :: [T.Text]
entryKeyOrder =
  [ "Timestamp"
  , "Id"
  , "KeyId"
  , "Description"
  , "Identity"
  , "Ciphertext"
  , "Meta"
  ]

entryOrdering :: Entry -> Entry -> Ordering
entryOrdering x y | _entryTimestamp   x /= _entryTimestamp   y = Ord.comparing _entryTimestamp   x y
                  | _entryId          x /= _entryId          y = Ord.comparing _entryId          x y
                  | _entryKeyId       x /= _entryKeyId       y = Ord.comparing _entryKeyId       x y
                  | _entryDescription x /= _entryDescription y = Ord.comparing _entryDescription x y
                  | _entryIdentity    x /= _entryIdentity    y = Ord.comparing _entryIdentity    x y
                  | _entryCiphertext  x /= _entryCiphertext  y = Ord.comparing _entryCiphertext  x y
                  | otherwise                                  = Ord.comparing _entryMeta        x y

instance Ord Entry where
  compare = entryOrdering

customOptions :: Options
customOptions = Aeson.defaultOptions{Aeson.fieldLabelModifier = strip}
  where
    strip :: String -> String
    strip str = Maybe.fromMaybe str (List.stripPrefix prefix str)

    prefix :: String
    prefix = "_entry"

instance FromJSON Entry where
  parseJSON = Aeson.genericParseJSON customOptions

instance ToJSON Entry where
  toJSON = Aeson.genericToJSON customOptions

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
showTime = T.pack . Format.formatTime Format.defaultTimeLocale "%s%Q"

ider :: T.Text -> Id
ider = Id . T.pack . SHA.showDigest . SHA.sha1 . BSL.fromStrict . Encoding.encodeUtf8

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
  let i = createId keyId timestamp description identity
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
  let i = createId keyId timestamp description identity
  return (Entry i keyId timestamp description identity ciphertext meta)

-- ** Their constituents

-- | A 'Id' identifies a given 'Entry'.
newtype Id = Id { unId :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Id where
  show (Id d) = show d

instance ToJSON Id where
  toJSON = toJSON . unId

instance FromJSON Id where
  parseJSON = fmap Id . parseJSON

instance ToField Id where
  toField (Id bs) = toField bs

instance FromField Id where
  fromField f = Id <$> fromField f

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = Description T.Text
  deriving (Eq, Ord, Generic)

instance Show Description where
  show (Description d) = show d

instance ToJSON Description where

instance FromJSON Description where

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
  deriving (Eq, Ord, Generic)

instance Show Identity where
  show (Identity i) = show i

instance ToJSON Identity where

instance FromJSON Identity where

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
  deriving (Eq, Ord, Generic)

instance Show Metadata where
  show (Metadata m) = show m

instance ToJSON Metadata where

instance FromJSON Metadata where

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
  Query { _queryId          = Id          . T.pack <$> i
        , _queryDescription = Description . T.pack <$> d
        , _queryIdentity    = Identity    . T.pack <$> iden
        , _queryMeta        = Metadata    . T.pack <$> m
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
