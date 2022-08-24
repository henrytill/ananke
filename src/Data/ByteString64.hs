{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.ByteString64
  ( ByteString64(..)
  , toText
  , fromText
  ) where

#if BACKEND_JSON
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (String))
import qualified Data.Aeson             as Aeson
#endif

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as T
import           Data.Text.Encoding
import           GHC.Generics


-- | Represents a ByteString which is displayed and stored in its base64-encoded
-- version
newtype ByteString64 = MkByteString64 { unByteString64 :: BS.ByteString }
  deriving (Eq, Ord, Generic)

toText :: ByteString64 -> T.Text
toText = decodeUtf8 . Base64.encode . unByteString64

fromText :: MonadFail m => T.Text -> m ByteString64
fromText = either fail (pure . MkByteString64) . Base64.decode . encodeUtf8

instance Show ByteString64 where
  show = T.unpack . toText

#if BACKEND_JSON
instance ToJSON ByteString64 where
  toJSON = String . toText

instance FromJSON ByteString64 where
  parseJSON = Aeson.withText "ByteString64" fromText
#endif
