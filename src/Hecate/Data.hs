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
  , Ciphertext
  , mkCiphertext
  , unCiphertext
  , ciphertextToText
  , ciphertextFromText
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
    -- * Helpers
  , utcTimeToText
  , utcTimeFromText
  ) where

import           Data.Aeson               (FromJSON (..), Options, ToJSON (..))
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.ByteString64        (ByteString64 (..))
import qualified Data.ByteString64        as BS64
import qualified Data.Digest.Pure.SHA     as SHA
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import           Data.Monoid              (First)
import qualified Data.Ord                 as Ord
import qualified Data.Semigroup           as Sem
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as Encoding
import           Data.Time.Clock          (UTCTime)
import qualified Data.Time.Format         as Format
import           Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import           GHC.Generics             (Generic)


-- * Configuration

data Backend = SQLite | JSON
  deriving (Eq, Show)

-- | A 'PreConfig' is used in the creation of a 'Config'
data PreConfig = MkPreConfig
  { preConfigDataDirectory     :: First FilePath
  , preConfigBackend           :: First Backend
  , preConfigKeyId             :: First KeyId
  , preConfigAllowMultipleKeys :: First Bool
  } deriving (Show, Eq)

instance Sem.Semigroup PreConfig where
  MkPreConfig a b c d <> MkPreConfig e f g h
    = MkPreConfig (a <> e)
                  (b <> f)
                  (c <> g)
                  (d <> h)

instance Monoid PreConfig where
  mempty
    = MkPreConfig mempty mempty mempty mempty

-- | A 'Config' represents our application's configuration
data Config = MkConfig
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
newtype SchemaVersion = MkSchemaVersion { unSchemaVersion :: Int }
  deriving Eq

instance Show SchemaVersion where
  show = show . unSchemaVersion

-- * Display Entry

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = MkDisplayEntry
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
    f ent plaintext = MkDisplayEntry (entryId ent)
                                     (entryTimestamp ent)
                                     (entryDescription ent)
                                     (entryIdentity ent)
                                     plaintext
                                     (entryMeta ent)

-- | A 'KeyId' represents a GPG Key Id
newtype KeyId = MkKeyId { unKeyId :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show KeyId where
  show (MkKeyId a) = show a

instance ToJSON KeyId where
  toJSON = toJSON . unKeyId

instance FromJSON KeyId where
  parseJSON = fmap MkKeyId . parseJSON

-- * Decrypted and encrypted values

-- | A 'Plaintext' represents a decrypted value
newtype Plaintext = MkPlaintext T.Text
  deriving Eq

instance Show Plaintext where
  show (MkPlaintext t) = show t

-- | A 'Ciphertext' represents an encrypted value
newtype Ciphertext = MkCiphertext ByteString64
  deriving (Show, Eq, Ord, Generic)

mkCiphertext :: BS.ByteString -> Ciphertext
mkCiphertext = MkCiphertext . MkByteString64

unCiphertext :: Ciphertext -> BS.ByteString
unCiphertext (MkCiphertext bs64) = unByteString64 bs64

ciphertextToText :: Ciphertext -> T.Text
ciphertextToText (MkCiphertext bs64) = BS64.toText bs64

ciphertextFromText :: MonadFail m => T.Text -> m Ciphertext
ciphertextFromText t = MkCiphertext <$> BS64.fromText t

instance ToJSON Ciphertext where

instance FromJSON Ciphertext where

-- * Entries

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = MkEntry
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

showTime :: UTCTime -> T.Text
showTime = T.pack . Format.formatTime Format.defaultTimeLocale "%s%Q"

ider :: T.Text -> Id
ider = MkId . T.pack . SHA.showDigest . SHA.sha1 . BSL.fromStrict . Encoding.encodeUtf8

createId :: KeyId -> UTCTime -> Description -> Maybe Identity -> Id
createId (MkKeyId k) ts (MkDescription d) (Just (MkIdentity i)) = ider (k <> showTime ts <> d <> i)
createId (MkKeyId k) ts (MkDescription d) Nothing               = ider (k <> showTime ts <> d)

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
  return (MkEntry i keyId timestamp description identity encrypted meta)

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
  return (MkEntry i keyId timestamp description identity ciphertext meta)

-- ** Their constituents

-- | A 'Id' identifies a given 'Entry'.
newtype Id = MkId { unId :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Id where
  show (MkId d) = show d

instance ToJSON Id where
  toJSON = toJSON . unId

instance FromJSON Id where
  parseJSON = fmap MkId . parseJSON

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = MkDescription { unDescription ::  T.Text }
  deriving (Eq, Ord, Generic)

instance Show Description where
  show (MkDescription d) = show d

instance ToJSON Description where
  toJSON = toJSON . unDescription

instance FromJSON Description where
  parseJSON = fmap MkDescription . parseJSON

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = MkIdentity { unIdentity :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Identity where
  show (MkIdentity i) = show i

instance ToJSON Identity where
  toJSON = toJSON . unIdentity

instance FromJSON Identity where
  parseJSON = fmap MkIdentity . parseJSON

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = MkMetadata { unMetadata :: T.Text }
  deriving (Eq, Ord, Generic)

instance Show Metadata where
  show (MkMetadata m) = show m

instance ToJSON Metadata where
  toJSON = toJSON . unMetadata

instance FromJSON Metadata where
  parseJSON = fmap MkMetadata . parseJSON

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
data Query = MkQuery
  { queryId          :: Maybe Id
  , queryDescription :: Maybe Description
  , queryIdentity    :: Maybe Identity
  , queryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

query :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Query
query i d iden m =
  MkQuery { queryId          = MkId          . T.pack <$> i
          , queryDescription = MkDescription . T.pack <$> d
          , queryIdentity    = MkIdentity    . T.pack <$> iden
          , queryMeta        = MkMetadata    . T.pack <$> m
          }

queryIsEmpty :: Query -> Bool
queryIsEmpty (MkQuery Nothing Nothing Nothing Nothing) = True
queryIsEmpty _                                         = False

-- * Helpers

utcTimeToText :: UTCTime -> T.Text
utcTimeToText = T.pack . iso8601Show

utcTimeFromText :: MonadFail m => T.Text -> m UTCTime
utcTimeFromText = iso8601ParseM . T.unpack
