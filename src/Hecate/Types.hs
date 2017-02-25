{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Types where

import Control.Monad.Except
import Crypto.Random.Entropy (getEntropy)
import Data.Aeson (ToJSON (..), FromJSON (..), genericToEncoding, defaultOptions)
import Data.Data
import Data.Text.Encoding
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite

newtype ByteString64 = ByteString64 { unByteString64 :: BS.ByteString }
  deriving (Eq, Ord, Data, Generic, Read, Typeable)

toBase64 :: BS.ByteString -> T.Text
toBase64 = decodeUtf8 . Base64.encode

instance Show ByteString64 where
  show (ByteString64 bs) = T.unpack (toBase64 bs)

instance ToJSON ByteString64 where
  toJSON (ByteString64 bs) =
    toJSON (toBase64 bs)

instance FromJSON ByteString64 where
  parseJSON o =
    parseJSON o >>= either fail (pure . ByteString64) . Base64.decode . encodeUtf8

instance ToField ByteString64 where
  toField (ByteString64 bs) = toField bs

instance FromField ByteString64 where
  fromField f = ByteString64 <$> fromField f

-- | A 'Nonce' is a value that is used, in conjunction with a 'MasterKey', to
-- encrypt and decrypt a given value
newtype Nonce = Nonce ByteString64
  deriving (Generic, Show, Eq)

instance ToJSON Nonce where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Nonce

instance ToField Nonce where
  toField (Nonce bs) = toField bs

instance FromField Nonce where
  fromField f = Nonce <$> fromField f

makeNonce :: MonadIO m => m Nonce
makeNonce = Nonce . ByteString64 <$> liftIO (getEntropy 12)

-- | An 'AuthTag' is additional authenticated data that is used to verify the
-- integrity of a decrypted value
newtype AuthTag = AuthTag ByteString64
  deriving (Generic, Show, Eq)

instance ToJSON AuthTag where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthTag

instance ToField AuthTag where
  toField (AuthTag bs) = toField bs

instance FromField AuthTag where
  fromField f = AuthTag <$> fromField f

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = Description T.Text
  deriving (Generic, Show, Eq)

instance ToJSON Description where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Description

instance ToField Description where
  toField (Description bs) = toField bs

instance FromField Description where
  fromField f = Description <$> fromField f

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = Identity T.Text
  deriving (Generic, Show, Eq)

instance ToJSON Identity where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Identity

instance ToField Identity where
  toField (Identity bs) = toField bs

instance FromField Identity where
  fromField f = Identity <$> fromField f

-- | A 'CipherText' represents an encrypted value
newtype CipherText = CipherText ByteString64
  deriving (Generic, Show, Eq)

instance ToJSON CipherText where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CipherText

instance ToField CipherText where
  toField (CipherText bs) = toField bs

instance FromField CipherText where
  fromField f = CipherText <$> fromField f

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = Metadata T.Text
  deriving (Generic, Show, Eq)

instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Metadata

instance ToField Metadata where
  toField (Metadata bs) = toField bs

instance FromField Metadata where
  fromField f = Metadata <$> fromField f

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = Entry
  { nonce       :: Nonce
  , authTag     :: AuthTag
  , timestamp   :: UTCTime
  , description :: Description
  , identity    :: Maybe Identity
  , cipherText  :: CipherText
  , meta        :: Maybe Metadata
  } deriving (Generic, Show, Eq)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

instance SQLite.FromRow Entry where
  fromRow = Entry <$> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field

instance SQLite.ToRow Entry where
  toRow (Entry nce at ts d i ct m) =
    SQLite.toRow (nce, at, ts, d, i, ct, m)

data Query = Query
  { queryNonce       :: Maybe Nonce
  , queryDescription :: Maybe Description
  , queryIdentity    :: Maybe Identity
  , queryMeta        :: Maybe Metadata
  } deriving (Generic, Show, Eq)

instance ToJSON Query where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Query

newtype Ok = Ok { msg :: String }
  deriving (Generic, Show)

instance ToJSON Ok where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ok
