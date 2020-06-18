module Hecate.GPG
  ( encrypt
  , decrypt
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           System.Exit
import qualified System.Process.ByteString as BSP

import           Hecate.Data               (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import           Hecate.Error


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
encryptWrapper f = (mkCiphertext <$>) . lifter . f . encodeUtf8

decryptWrapper
  :: (MonadThrow m, MonadIO m)
  => (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString))
  -> Ciphertext
  -> m Plaintext
decryptWrapper f = (Plaintext . decodeUtf8 <$>) . lifter . f . unCiphertext

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
decrypt = decryptWrapper gpgDecrypt
