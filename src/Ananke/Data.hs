{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Ananke.Data
  ( -- * Configuration
    Backend (..),
    PreConfig (..),
    Config (..),
    configDatabaseDir,
    configSchemaFile,
    configDatabaseFile,
    configDataFile,
    SchemaVersion (..),
    KeyId (..),

    -- * Decrypted and encrypted values
    Plaintext (..),
    mkPlaintext,
    Ciphertext,
    mkCiphertext,
    unCiphertext,
    ciphertextToText,
    ciphertextFromText,

    -- * Entries
    Entry (..),
    entryKeyOrder,

    -- ** their constituents
    Id (..),
    Description (..),
    Identity (..),
    Metadata (..),

    -- ** and related
    mkEntry,
    updateEntry,

    -- * Display Entries
    DisplayEntry (..),

    -- * Queries
    Query (..),
    emptyQuery,
    queryIsEmpty,

    -- * Helpers
    utcTimeToText,
    utcTimeFromText,
  )
where

import Data.Aeson (FromJSON (..), Options, ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString64 (ByteString64 (..))
import qualified Data.ByteString64 as BS64
import qualified Data.Digest.Pure.SHA as SHA
import Data.Monoid (First)
import qualified Data.Ord as Ord
import qualified Data.Semigroup as Sem
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- * Configuration

data Backend = SQLite | JSON
  deriving (Eq, Show)

-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = MkPreConfig
  { preConfigDir :: First FilePath,
    preConfigDataDir :: First FilePath,
    preConfigBackend :: First Backend,
    preConfigKeyId :: First KeyId,
    preConfigMultKeys :: First Bool
  }
  deriving (Show, Eq)

instance Sem.Semigroup PreConfig where
  MkPreConfig a b c d e <> MkPreConfig n o p q r =
    MkPreConfig (a <> n) (b <> o) (c <> p) (d <> q) (e <> r)

instance Monoid PreConfig where
  mempty = MkPreConfig mempty mempty mempty mempty mempty

-- | A 'Config' represents our application's configuration
data Config = MkConfig
  { configDir :: FilePath,
    configDataDir :: FilePath,
    configBackend :: Backend,
    configKeyId :: KeyId,
    configMultKeys :: Bool
  }
  deriving (Show, Eq)

-- Virtual fields
configDatabaseDir :: Config -> FilePath
configDatabaseDir cfg = configDataDir cfg ++ "/db"

configSchemaFile :: Config -> FilePath
configSchemaFile cfg = configDatabaseDir cfg ++ "/schema"

configDatabaseFile :: Config -> FilePath
configDatabaseFile cfg = configDatabaseDir cfg ++ "/db.sqlite"

configDataFile :: Config -> FilePath
configDataFile cfg = configDatabaseDir cfg ++ "/data.json"

-- | A 'SchemaVersion' represents the database's schema version
newtype SchemaVersion = MkSchemaVersion {unSchemaVersion :: Int}
  deriving (Eq)

instance Show SchemaVersion where
  show = show . unSchemaVersion

-- | A 'KeyId' represents a GPG Key Id
newtype KeyId = MkKeyId {unKeyId :: T.Text}
  deriving (Eq, Ord)

instance Show KeyId where
  show (MkKeyId a) = show a

instance ToJSON KeyId where
  toJSON = toJSON . unKeyId

instance FromJSON KeyId where
  parseJSON = fmap MkKeyId . parseJSON

-- * Decrypted and encrypted values

-- | A 'Plaintext' represents a decrypted value
newtype Plaintext = MkPlaintext T.Text
  deriving (Eq)

instance Show Plaintext where
  show (MkPlaintext t) = show t

mkPlaintext :: String -> Plaintext
mkPlaintext = MkPlaintext . T.pack

-- | A 'Ciphertext' represents an encrypted value
newtype Ciphertext = MkCiphertext ByteString64
  deriving (Show, Eq, Ord)

mkCiphertext :: BS.ByteString -> Ciphertext
mkCiphertext = MkCiphertext . MkByteString64

unCiphertext :: Ciphertext -> BS.ByteString
unCiphertext (MkCiphertext bs64) = unByteString64 bs64

ciphertextToText :: Ciphertext -> T.Text
ciphertextToText (MkCiphertext bs64) = BS64.toText bs64

ciphertextFromText :: (MonadFail m) => T.Text -> m Ciphertext
ciphertextFromText t = MkCiphertext <$> BS64.fromText t

instance ToJSON Ciphertext where
  toJSON (MkCiphertext b) = toJSON b

instance FromJSON Ciphertext where
  parseJSON = fmap MkCiphertext . parseJSON

-- * Entries

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = MkEntry
  { entryId :: Id,
    entryKeyId :: KeyId,
    entryTimestamp :: UTCTime,
    entryDescription :: Description,
    entryIdentity :: Maybe Identity,
    entryCiphertext :: Ciphertext,
    entryMeta :: Maybe Metadata
  }
  deriving (Show, Eq, Generic)

instance Ord Entry where
  compare x y
    | entryTimestamp x /= entryTimestamp y = Ord.comparing entryTimestamp x y
    | entryId x /= entryId y = Ord.comparing entryId x y
    | entryKeyId x /= entryKeyId y = Ord.comparing entryKeyId x y
    | entryDescription x /= entryDescription y = Ord.comparing entryDescription x y
    | entryIdentity x /= entryIdentity y = Ord.comparing entryIdentity x y
    | entryCiphertext x /= entryCiphertext y = Ord.comparing entryCiphertext x y
    | otherwise = Ord.comparing entryMeta x y

fieldToJSON :: [(String, String)]
fieldToJSON =
  [ ("entryTimestamp", "timestamp"),
    ("entryId", "id"),
    ("entryKeyId", "keyId"),
    ("entryDescription", "description"),
    ("entryIdentity", "identity"),
    ("entryCiphertext", "ciphertext"),
    ("entryMeta", "meta")
  ]

entryKeyOrder :: [T.Text]
entryKeyOrder = map (T.pack . snd) fieldToJSON

remapField :: String -> String
remapField field
  | Just mapped <- lookup field fieldToJSON = mapped
  | otherwise = field

options :: Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = remapField,
      Aeson.omitNothingFields = True
    }

instance ToJSON Entry where
  toJSON = Aeson.genericToJSON options

instance FromJSON Entry where
  parseJSON = Aeson.genericParseJSON options

-- ** their constituents

-- | A 'Id' identifies a given 'Entry'.
newtype Id = MkId {unId :: T.Text}
  deriving (Eq, Ord)

instance Show Id where
  show (MkId d) = show d

instance ToJSON Id where
  toJSON = toJSON . unId

instance FromJSON Id where
  parseJSON = fmap MkId . parseJSON

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = MkDescription {unDescription :: T.Text}
  deriving (Eq, Ord)

instance Show Description where
  show (MkDescription d) = show d

instance ToJSON Description where
  toJSON = toJSON . unDescription

instance FromJSON Description where
  parseJSON = fmap MkDescription . parseJSON

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = MkIdentity {unIdentity :: T.Text}
  deriving (Eq, Ord)

instance Show Identity where
  show (MkIdentity i) = show i

instance ToJSON Identity where
  toJSON = toJSON . unIdentity

instance FromJSON Identity where
  parseJSON = fmap MkIdentity . parseJSON

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = MkMetadata {unMetadata :: T.Text}
  deriving (Eq, Ord)

instance Show Metadata where
  show (MkMetadata m) = show m

instance ToJSON Metadata where
  toJSON = toJSON . unMetadata

instance FromJSON Metadata where
  parseJSON = fmap MkMetadata . parseJSON

-- ** and related

mkId :: T.Text -> Id
mkId = MkId . T.pack . SHA.showDigest . SHA.sha1 . BSL.fromStrict . Encoding.encodeUtf8

generateId :: KeyId -> UTCTime -> Description -> Maybe Identity -> Id
generateId (MkKeyId k) t (MkDescription d) (Just (MkIdentity i)) =
  mkId $ k <> utcTimeToText t <> d <> i
generateId (MkKeyId k) t (MkDescription d) Nothing =
  mkId $ k <> utcTimeToText t <> d

mkEntry :: KeyId -> UTCTime -> Description -> Maybe Identity -> Ciphertext -> Maybe Metadata -> Entry
mkEntry keyId timestamp description identity ciphertext meta =
  MkEntry id keyId timestamp description identity ciphertext meta
  where
    id = generateId keyId timestamp description identity

updateEntry :: Entry -> Entry
updateEntry entry@(MkEntry _ keyId timestamp description identity _ _) =
  entry {entryId = id}
  where
    id = generateId keyId timestamp description identity

-- * Display Entry

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = MkDisplayEntry
  { displayId :: Id,
    displayTimestamp :: UTCTime,
    displayDescription :: Description,
    displayIdentity :: Maybe Identity,
    displayPlaintext :: Plaintext,
    displayMeta :: Maybe Metadata
  }
  deriving (Show, Eq)

-- * Queries

-- | A 'Query' represents a database query
data Query = MkQuery
  { queryId :: Maybe Id,
    queryDescription :: Maybe Description,
    queryIdentity :: Maybe Identity,
    queryMeta :: Maybe Metadata
  }
  deriving (Show, Eq)

emptyQuery :: Query
emptyQuery = MkQuery Nothing Nothing Nothing Nothing

queryIsEmpty :: Query -> Bool
queryIsEmpty (MkQuery Nothing Nothing Nothing Nothing) = True
queryIsEmpty _ = False

-- * Helpers

utcTimeToText :: UTCTime -> T.Text
utcTimeToText = T.pack . iso8601Show

utcTimeFromText :: (MonadFail m) => T.Text -> m UTCTime
utcTimeFromText = iso8601ParseM . T.unpack
