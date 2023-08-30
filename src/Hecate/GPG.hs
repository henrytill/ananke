{-# LANGUAGE ScopedTypeVariables #-}

module Hecate.GPG
  ( encrypt
  , decrypt
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as Encoding
import           System.Exit            (ExitCode (..))

import           Hecate.Data            (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import           Hecate.Error           (AppError (..))
import           Hecate.GPG.Process     (readProcessWithExitCode)


gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt ::           BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]
gpgDecrypt       = readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

convertResult
  :: (MonadThrow m, MonadIO m)
  => (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
convertResult (ExitSuccess  , stdout, _     ) = return stdout
convertResult (ExitFailure _, _     , stderr) = throwM . GPG . T.unpack . Encoding.decodeUtf8 $ stderr

lifter
  :: (MonadThrow m, MonadIO m)
  => IO (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
lifter x = liftIO x >>= convertResult

encrypt :: forall m. (MonadThrow m, MonadIO m) => KeyId -> Plaintext -> m Ciphertext
encrypt (MkKeyId keyid) (MkPlaintext pt) = w (gpgEncrypt (T.unpack keyid)) pt
  where
    w :: (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)) -> T.Text -> m Ciphertext
    w f = (mkCiphertext <$>) . lifter . f . Encoding.encodeUtf8

decrypt :: forall m. (MonadThrow m, MonadIO m) => Ciphertext -> m Plaintext
decrypt = w gpgDecrypt
  where
    w :: (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)) -> Ciphertext -> m Plaintext
    w f = (MkPlaintext . Encoding.decodeUtf8 <$>) . lifter . f . unCiphertext
