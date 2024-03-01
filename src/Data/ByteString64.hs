module Data.ByteString64
  ( ByteString64 (..),
    toText,
    fromText,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding

-- | A ByteString which is displayed and serialized in its base64 encoding
newtype ByteString64 = MkByteString64 {unByteString64 :: ByteString}
  deriving (Eq, Ord)

toText :: ByteString64 -> Text
toText = decodeUtf8 . Base64.encode . unByteString64

fromText :: (MonadFail m) => Text -> m ByteString64
fromText = either fail (return . MkByteString64) . Base64.decode . encodeUtf8

instance Show ByteString64 where
  show = Text.unpack . toText

instance ToJSON ByteString64 where
  toJSON = String . toText

instance FromJSON ByteString64 where
  parseJSON = Aeson.withText "ByteString64" fromText
