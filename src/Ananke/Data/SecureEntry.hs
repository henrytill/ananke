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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | A record that stores a plaintext value along with associated information.
data SecureEntry = MkSecureEntry
  { -- | The entry creation time.
    entryTimestamp :: UTCTime,
    -- | A URI or a descriptive name.
    entryDescription :: Description,
    -- | An optional identifying value, such as a username.
    entryIdentity :: Maybe Identity,
    -- | The plaintext value.
    entryPlaintext :: Plaintext,
    -- | Additional non-specific information.
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

secureEntryKeyOrder :: [Text]
secureEntryKeyOrder = map (Text.pack . snd) fieldToJSON

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
