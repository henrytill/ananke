module Hecate.GPG
  ( KeyId(..)
    -- * Decrypted and encrypted values
  , Plaintext(..)
  , Ciphertext
    -- * Functions on them
  , encrypt
  , decrypt
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString                  as BS
import           Data.ByteString64
import qualified Data.Csv                         as CSV
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           System.Exit
import qualified System.Process.ByteString        as BSP

import           Hecate.Error


-- | A 'KeyId' represents a GPG Key Id
newtype KeyId = KeyId { unKeyId :: T.Text }
  deriving Eq

instance Show KeyId where
  show (KeyId a) = show a

instance ToField KeyId where
  toField (KeyId bs) = toField bs

instance FromField KeyId where
  fromField f = KeyId <$> fromField f

-- * Decrypted and encrypted values

-- | A 'Plaintext' represents a decrypted value
newtype Plaintext = Plaintext T.Text
  deriving Eq

instance Show Plaintext where
  show (Plaintext t) = show t

instance CSV.ToField Plaintext where
  toField (Plaintext bs) = CSV.toField bs

instance CSV.FromField Plaintext where
  parseField f = Plaintext <$> CSV.parseField f

-- | A 'Ciphertext' represents an encrypted value
newtype Ciphertext = Ciphertext ByteString64
  deriving (Show, Eq)

instance ToField Ciphertext where
  toField (Ciphertext bs) = toField bs

instance FromField Ciphertext where
  fromField f = Ciphertext <$> fromField f

-- * Functions on them

gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt ::           BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = BSP.readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]
gpgDecrypt       = BSP.readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

convertResult
  :: (MonadThrow m, MonadIO m)
  => (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
convertResult (ExitSuccess  , stdout, _     ) = pure stdout
convertResult (ExitFailure _, _     , stderr) = perr stderr
  where
    perr x = throwM (GPG (T.unpack (decodeUtf8 x)))

lifter
  :: (MonadThrow m, MonadIO m)
  => IO (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
lifter x = liftIO x >>= convertResult

encryptWrapper
  :: (MonadThrow m, MonadIO m)
  => (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString))
  -> T.Text
  -> m Ciphertext
encryptWrapper f = (Ciphertext . ByteString64 <$>) . lifter . f . encodeUtf8

decryptWrapper
  :: (MonadThrow m, MonadIO m)
  => (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString))
  -> ByteString64
  -> m Plaintext
decryptWrapper f = (Plaintext . decodeUtf8 <$>) . lifter . f . unByteString64

encrypt
  :: (MonadThrow m, MonadIO m)
  => KeyId
  -> Plaintext
  -> m Ciphertext
encrypt (KeyId keyid) (Plaintext pt) = encryptWrapper (gpgEncrypt (T.unpack keyid)) pt

decrypt
  :: (MonadThrow m, MonadIO m)
  => Ciphertext
  -> m Plaintext
decrypt (Ciphertext ct) = decryptWrapper gpgDecrypt ct
