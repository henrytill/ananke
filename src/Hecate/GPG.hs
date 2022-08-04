{-# LANGUAGE ScopedTypeVariables #-}

module Hecate.GPG
  ( encrypt
  , decrypt
  ) where

import           Control.Monad.Catch       (MonadThrow (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as Encoding
import           System.Exit               (ExitCode (..))
import qualified System.Process.ByteString as BSP

import           Hecate.Data               (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import           Hecate.Error              (AppError (..))


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
    perr x = throwM (GPG (T.unpack (Encoding.decodeUtf8 x)))

lifter
  :: (MonadThrow m, MonadIO m)
  => IO (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
lifter x = liftIO x >>= convertResult

encrypt
  :: forall m. (MonadThrow m, MonadIO m)
  => KeyId
  -> Plaintext
  -> m Ciphertext
encrypt (KeyId keyid) (Plaintext pt) = w (gpgEncrypt (T.unpack keyid)) pt
  where
    w :: (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)) -> T.Text -> m Ciphertext
    w f = (mkCiphertext <$>) . lifter . f . Encoding.encodeUtf8

decrypt
  :: forall m. (MonadThrow m, MonadIO m)
  => Ciphertext
  -> m Plaintext
decrypt = w gpgDecrypt
  where
    w ::  (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)) -> Ciphertext -> m Plaintext
    w f = (Plaintext . Encoding.decodeUtf8 <$>) . lifter . f . unCiphertext
