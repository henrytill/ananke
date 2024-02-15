module Ananke.Data.Core
  ( -- ** Configuration
    Backend (..),
    PreConfig (..),
    Config (..),
    configFile,
    configDatabaseDir,
    configSchemaFile,
    configDatabaseFile,
    configDataFile,
    SchemaVersion (..),
    KeyId (..),

    -- ** Decrypted values
    Plaintext (..),
    mkPlaintext,

    -- ** Entry constiuents
    Id (..),
    Description (..),
    Identity (..),
    Metadata (..),
    generateId,

    -- ** Display of Entries
    DisplayEntry (..),

    -- ** Query
    Query (..),
    emptyQuery,
    queryIsEmpty,

    -- ** Helpers
    utcTimeToText,
    utcTimeFromText,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString.Lazy qualified as BSL
import Data.Digest.Pure.SHA qualified as SHA
import Data.Monoid (First)
import Data.Semigroup qualified as Sem
import Data.Text qualified as T
import Data.Text.Encoding qualified as Encoding
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import System.FilePath ((<.>), (</>))
import Prelude hiding (id)

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
configFile :: Config -> FilePath
configFile cfg = configDir cfg </> "ananke" <.> "ini"

configDatabaseDir :: Config -> FilePath
configDatabaseDir cfg = configDataDir cfg </> "db"

configSchemaFile :: Config -> FilePath
configSchemaFile cfg = configDatabaseDir cfg </> "schema"

configDatabaseFile :: Config -> FilePath
configDatabaseFile cfg = configDatabaseDir cfg </> "db" <.> "sqlite"

configDataFile :: Config -> FilePath
configDataFile cfg = configDatabaseDir cfg </> "data" <.> "json"

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

-- | A 'Plaintext' represents a decrypted value
newtype Plaintext = MkPlaintext T.Text
  deriving (Eq)

instance Show Plaintext where
  show (MkPlaintext t) = show t

mkPlaintext :: String -> Plaintext
mkPlaintext = MkPlaintext . T.pack

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

mkId :: T.Text -> Id
mkId = MkId . T.pack . SHA.showDigest . SHA.sha1 . BSL.fromStrict . Encoding.encodeUtf8

generateId :: KeyId -> UTCTime -> Description -> Maybe Identity -> Id
generateId (MkKeyId k) t (MkDescription d) (Just (MkIdentity i)) =
  mkId $ k <> utcTimeToText t <> d <> i
generateId (MkKeyId k) t (MkDescription d) Nothing =
  mkId $ k <> utcTimeToText t <> d

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = MkDisplayEntry
  { displayTimestamp :: UTCTime,
    displayId :: Id,
    displayKeyId :: KeyId,
    displayDescription :: Description,
    displayIdentity :: Maybe Identity,
    displayPlaintext :: Plaintext,
    displayMeta :: Maybe Metadata
  }
  deriving (Show, Eq)

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

utcTimeToText :: UTCTime -> T.Text
utcTimeToText = T.pack . iso8601Show

utcTimeFromText :: (MonadFail m) => T.Text -> m UTCTime
utcTimeFromText = iso8601ParseM . T.unpack
