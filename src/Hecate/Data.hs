{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Data
  ( -- * Configuration
    Backend(..)
  , PreConfig(..)
  , Config(..)
  , configDatabaseDirectory
  , configSchemaFile
  , configDatabaseFile
  , configDataFile
  , SchemaVersion(..)
    -- * Import & Display Entries
  , DisplayEntry(..)
  , entryToDisplayEntry
  , KeyId(..)
    -- * Decrypted and encrypted values
  , Plaintext(..)
  , Ciphertext(..)
  , mkCiphertext
  , unCiphertext
    -- * Entries
  , Entry(..)
  , entryKeyOrder
  , createEntry
    -- ** Their constituents
  , Id(..)
  , Description(..)
  , Identity(..)
  , Metadata(..)
    -- ** And some updaters
  , updateKeyId
  , updateDescription
  , updateIdentity
  , updateMetadata
    -- * Queries
  , Query(..)
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


-- * Configuration

data Backend = SQLiteSimple | SQLite | JSON
  deriving (Eq, Show)

-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = PreConfig
  { preConfigDataDirectory     :: First FilePath
  , preConfigBackend           :: First Backend
  , preConfigKeyId             :: First KeyId
  , preConfigAllowMultipleKeys :: First Bool
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
  { configDataDirectory     :: FilePath
  , configBackend           :: Backend
  , configKeyId             :: KeyId
  , configAllowMultipleKeys :: Bool
  } deriving (Show, Eq)

configDatabaseDirectory :: Config -> FilePath
configSchemaFile        :: Config -> FilePath
configDatabaseFile      :: Config -> FilePath
configDataFile          :: Config -> FilePath
configDatabaseDirectory cfg = configDataDirectory     cfg ++ "/db"
configSchemaFile        cfg = configDatabaseDirectory cfg ++ "/schema"
configDatabaseFile      cfg = configDatabaseDirectory cfg ++ "/db.sqlite"
configDataFile          cfg = configDatabaseDirectory cfg ++ "/data.json"

-- | A 'SchemaVersion' represents the database's schema version
newtype SchemaVersion = SchemaVersion { unSchemaVersion :: Int }
  deriving Eq

instance Show SchemaVersion where
  show = show . unSchemaVersion

-- * Display Entry

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = DisplayEntry
  { displayId          :: Id
  , displayTimestamp   :: UTCTime
  , displayDescription :: Description
  , displayIdentity    :: Maybe Identity
  , displayPlaintext   :: Plaintext
  , displayMeta        :: Maybe Metadata
  } deriving (Show, Eq)

entryToDisplayEntry
  :: Monad m
  => (Ciphertext -> m Plaintext)
  -> Entry
  -> m DisplayEntry
entryToDisplayEntry decrypt e
  = f e <$> decrypt (entryCiphertext e)
  where
    f ent plaintext = DisplayEntry (entryId ent)
                                   (entryTimestamp ent)
                                   (entryDescription ent)
                                   (entryIdentity ent)
                                   plaintext
                                   (entryMeta ent)

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
  { entryId          :: Id
  , entryKeyId       :: KeyId
  , entryTimestamp   :: UTCTime
  , entryDescription :: Description
  , entryIdentity    :: Maybe Identity
  , entryCiphertext  :: Ciphertext
  , entryMeta        :: Maybe Metadata
  } deriving (Show, Eq, Generic)

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
entryOrdering x y | entryTimestamp   x /= entryTimestamp   y = Ord.comparing entryTimestamp   x y
                  | entryId          x /= entryId          y = Ord.comparing entryId          x y
                  | entryKeyId       x /= entryKeyId       y = Ord.comparing entryKeyId       x y
                  | entryDescription x /= entryDescription y = Ord.comparing entryDescription x y
                  | entryIdentity    x /= entryIdentity    y = Ord.comparing entryIdentity    x y
                  | entryCiphertext  x /= entryCiphertext  y = Ord.comparing entryCiphertext  x y
                  | otherwise                                = Ord.comparing entryMeta        x y

instance Ord Entry where
  compare = entryOrdering

customOptions :: Options
customOptions = Aeson.defaultOptions{Aeson.fieldLabelModifier = strip}
  where
    strip :: String -> String
    strip str = Maybe.fromMaybe str (List.stripPrefix prefix str)

    prefix :: String
    prefix = "entry"

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
  toRow ent = SQLite.toRow ( entryId          ent
                           , entryKeyId       ent
                           , entryTimestamp   ent
                           , entryDescription ent
                           , entryIdentity    ent
                           , entryCiphertext  ent
                           , entryMeta        ent
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
newtype Description = Description { unDescription ::  T.Text }
  deriving (Eq, Ord, Generic)

instance Show Description where
  show (Description d) = show d

instance ToJSON Description where
  toJSON = toJSON . unDescription

instance FromJSON Description where
  parseJSON = fmap Description . parseJSON

instance ToField Description where
  toField (Description bs) = toField bs

instance FromField Description where
  fromField f = Description <$> fromField f

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = Identity { unIdentity :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Identity where
  show (Identity i) = show i

instance ToJSON Identity where
  toJSON = toJSON . unIdentity

instance FromJSON Identity where
  parseJSON = fmap Identity . parseJSON

instance ToField Identity where
  toField (Identity bs) = toField bs

instance FromField Identity where
  fromField f = Identity <$> fromField f

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = Metadata { unMetadata :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Metadata where
  show (Metadata m) = show m

instance ToJSON Metadata where
  toJSON = toJSON . unMetadata

instance FromJSON Metadata where
  parseJSON = fmap Metadata . parseJSON

instance ToField Metadata where
  toField (Metadata bs) = toField bs

instance FromField Metadata where
  fromField f = Metadata <$> fromField f

-- ** And some updaters

updateKeyId
  :: Monad m
  => (Ciphertext -> m Plaintext)
  -> (KeyId -> Plaintext -> m Ciphertext)
  -> KeyId
  -> Entry
  -> m Entry
updateKeyId decrypt encrypt keyId ent = do
  plaintext <- decrypt (entryCiphertext ent)
  createEntry encrypt
              keyId
              (entryTimestamp ent)
              (entryDescription ent)
              (entryIdentity ent)
              plaintext
              (entryMeta ent)

updateDescription
  :: Monad m
  => UTCTime
  -> Description
  -> Entry
  -> m Entry
updateDescription now desc ent
  = updateEntry (entryKeyId ent)
                now
                desc
                (entryIdentity ent)
                (entryCiphertext ent)
                (entryMeta ent)

updateIdentity
  :: Monad m
  => UTCTime
  -> Maybe Identity
  -> Entry
  -> m Entry
updateIdentity now iden ent
  = updateEntry (entryKeyId ent)
                now
                (entryDescription ent)
                iden
                (entryCiphertext ent)
                (entryMeta ent)

updateMetadata
  :: Monad m
  => UTCTime
  -> Maybe Metadata
  -> Entry
  -> m Entry
updateMetadata now meta ent
  = updateEntry (entryKeyId ent)
                now
                (entryDescription ent)
                (entryIdentity ent)
                (entryCiphertext ent)
                meta

-- * Queries

-- | A 'Query' represents a database query
data Query = Query
  { queryId          :: Maybe Id
  , queryDescription :: Maybe Description
  , queryIdentity    :: Maybe Identity
  , queryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

query :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Query
query i d iden m =
  Query { queryId          = Id          . T.pack <$> i
        , queryDescription = Description . T.pack <$> d
        , queryIdentity    = Identity    . T.pack <$> iden
        , queryMeta        = Metadata    . T.pack <$> m
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
