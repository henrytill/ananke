{-# LANGUAGE DeriveGeneric #-}

module Ananke.Data.Entry
  ( -- ** Ciphertext
    Ciphertext,
    mkCiphertext,
    unCiphertext,
    ciphertextToText,
    ciphertextFromText,

    -- ** Entry
    Entry (..),
    entryKeyOrder,
    mkEntry,
    updateEntry,
  )
where

import Ananke.Data.Common (Description, Id, Identity, KeyId, Metadata)
import Ananke.Data.Common qualified as Common
import Data.Aeson (FromJSON (..), Options, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString64 (ByteString64 (..))
import Data.ByteString64 qualified as ByteString64
import Data.Ord qualified as Ord
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- | A 'Ciphertext' represents an encrypted value
newtype Ciphertext = MkCiphertext ByteString64
  deriving (Show, Eq, Ord)

mkCiphertext :: ByteString -> Ciphertext
mkCiphertext = MkCiphertext . MkByteString64

unCiphertext :: Ciphertext -> ByteString
unCiphertext (MkCiphertext bs64) = unByteString64 bs64

ciphertextToText :: Ciphertext -> Text
ciphertextToText (MkCiphertext bs64) = ByteString64.toText bs64

ciphertextFromText :: (MonadFail m) => Text -> m Ciphertext
ciphertextFromText t = MkCiphertext <$> ByteString64.fromText t

instance ToJSON Ciphertext where
  toJSON (MkCiphertext b) = toJSON b

instance FromJSON Ciphertext where
  parseJSON = fmap MkCiphertext . parseJSON

-- | A record that stores an encrypted value along with associated information.
data Entry = MkEntry
  { -- | The entry creation time.
    entryTimestamp :: UTCTime,
    -- | A unique identifier.
    entryId :: Id,
    -- | The GPG key id used to encrypt the ciphertext.
    entryKeyId :: KeyId,
    -- | A URI or descriptive name.
    entryDescription :: Description,
    -- | An optional identifying value, such as a username.
    entryIdentity :: Maybe Identity,
    -- | The encrypted value.
    entryCiphertext :: Ciphertext,
    -- | Additional non-specific information.
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

entryKeyOrder :: [Text]
entryKeyOrder = map (Text.pack . snd) fieldToJSON

options :: Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Common.remapField fieldToJSON,
      Aeson.omitNothingFields = True
    }

instance ToJSON Entry where
  toJSON = Aeson.genericToJSON options

instance FromJSON Entry where
  parseJSON = Aeson.genericParseJSON options

mkEntry :: UTCTime -> KeyId -> Description -> Maybe Identity -> Ciphertext -> Maybe Metadata -> Entry
mkEntry timestamp keyId description identity ciphertext meta =
  MkEntry timestamp id keyId description identity ciphertext meta
  where
    id = Common.generateId keyId timestamp description identity

updateEntry :: Entry -> Entry
updateEntry entry@(MkEntry timestamp _ keyId description identity _ _) =
  entry {entryId = id}
  where
    id = Common.generateId keyId timestamp description identity
