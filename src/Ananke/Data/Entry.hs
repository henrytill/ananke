{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

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

import Ananke.Data.Core (Description, Id, Identity, KeyId, Metadata, generateId)
import Data.Aeson (FromJSON (..), Options, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString64 (ByteString64 (..))
import Data.ByteString64 qualified as BS64
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Prelude hiding (id)

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

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = MkEntry
  { entryTimestamp :: UTCTime,
    entryId :: Id,
    entryKeyId :: KeyId,
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

remapField :: [(String, String)] -> String -> String
remapField mappings field
  | Just mapped <- lookup field mappings = mapped
  | otherwise = field

options :: Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = remapField fieldToJSON,
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
    id = generateId keyId timestamp description identity

updateEntry :: Entry -> Entry
updateEntry entry@(MkEntry timestamp _ keyId description identity _ _) =
  entry {entryId = id}
  where
    id = generateId keyId timestamp description identity
