{-# LANGUAGE DeriveGeneric #-}

module Data.ByteString64
  ( ByteString64(..)
  ) where

import           Data.Aeson                       (FromJSON (..), ToJSON (..), Value (String))
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64           as Base64
import qualified Data.Text                        as T
import           Data.Text.Encoding
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Generics


-- | Represents a ByteString which is displayed and stored in its base64-encoded
-- version
newtype ByteString64 = ByteString64 { unByteString64 :: BS.ByteString }
  deriving (Eq, Ord, Generic)

toBase64 :: BS.ByteString -> T.Text
toBase64 = decodeUtf8 . Base64.encode

instance Show ByteString64 where
  show (ByteString64 bs) = T.unpack (toBase64 bs)

instance ToJSON ByteString64 where
  toJSON (ByteString64 bs) = String (toBase64 bs)

instance FromJSON ByteString64 where
  parseJSON = Aeson.withText "ByteString64" $ either fail (pure . ByteString64) . Base64.decode . encodeUtf8

instance ToField ByteString64 where
  toField (ByteString64 bs) = toField (toBase64 bs)

instance FromField ByteString64 where
  fromField f = fromField f >>= either fail (pure . ByteString64) . Base64.decode . encodeUtf8
