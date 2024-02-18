{-# LANGUAGE DeriveGeneric #-}

module Ananke.Data.SecureEntry
  ( SecureEntry (..),
    secureEntryKeyOrder,
  )
where

import Ananke.Data.Common (Description, Identity, Metadata, Plaintext)
import Ananke.Data.Common qualified as Common
import Data.Aeson (FromJSON (..), Options, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | A record that stores a plaintext value along with associated information.
data SecureEntry = MkSecureEntry
  { -- | The time the entry was created
    entryTimestamp :: UTCTime,
    -- | Description of the entry. Can be a URI or a descriptive name.
    entryDescription :: Description,
    -- | Optional identifying value, such as a username.
    entryIdentity :: Maybe Identity,
    -- | Holds the plaintext value of the entry.
    entryPlaintext :: Plaintext,
    -- | Optional field for additional non-specific information.
    entryMeta :: Maybe Metadata
  }
  deriving (Show, Eq, Generic)

instance Ord SecureEntry where
  compare x y
    | entryTimestamp x /= entryTimestamp y = Ord.comparing entryTimestamp x y
    | entryDescription x /= entryDescription y = Ord.comparing entryDescription x y
    | entryIdentity x /= entryIdentity y = Ord.comparing entryIdentity x y
    | entryPlaintext x /= entryPlaintext y = Ord.comparing entryPlaintext x y
    | otherwise = Ord.comparing entryMeta x y

fieldToJSON :: [(String, String)]
fieldToJSON =
  [ ("entryTimestamp", "timestamp"),
    ("entryDescription", "description"),
    ("entryIdentity", "identity"),
    ("entryPlaintext", "plaintext"),
    ("entryMeta", "meta")
  ]

secureEntryKeyOrder :: [T.Text]
secureEntryKeyOrder = map (T.pack . snd) fieldToJSON

options :: Options
options =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Common.remapField fieldToJSON,
      Aeson.omitNothingFields = True
    }

instance ToJSON SecureEntry where
  toJSON = Aeson.genericToJSON options

instance FromJSON SecureEntry where
  parseJSON = Aeson.genericParseJSON options
