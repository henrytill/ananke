{-# LANGUAGE FlexibleContexts #-}

module Hecate.GPG where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text.Encoding
import Hecate.Types
import System.Exit
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified System.Process.ByteString as BSP

gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt ::           BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = BSP.readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]
gpgDecrypt       = BSP.readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

convertResult
  :: (MonadIO m, MonadError AppError m)
  => (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
convertResult (ExitSuccess  , stdout, _     ) = pure stdout
convertResult (ExitFailure _, _     , stderr) = perr stderr
  where
    perr = throwError . GPG . T.unpack . decodeUtf8

lifter
  :: (MonadIO m, MonadError AppError m)
  => IO (ExitCode, BS.ByteString, BS.ByteString)
  -> m BS.ByteString
lifter x = liftIO x >>= convertResult

encryptWrapper
  :: (MonadIO m, MonadError AppError m)
  => (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString))
  -> T.Text
  -> m Ciphertext
encryptWrapper f = (Ciphertext . ByteString64 <$>) . lifter . f . encodeUtf8

decryptWrapper
  :: (MonadIO m, MonadError AppError m)
  => (BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString))
  -> ByteString64
  -> m Plaintext
decryptWrapper f = (Plaintext . decodeUtf8 <$>) . lifter . f . unByteString64

encryptM
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Plaintext
  -> m Ciphertext
encryptM (Plaintext pt) = do
  ctx <- ask
  let (KeyId keyid) = _keyId ctx
  encryptWrapper (gpgEncrypt (T.unpack keyid)) pt

decryptM
  :: (MonadIO m, MonadError AppError m)
  => Ciphertext
  -> m Plaintext
decryptM (Ciphertext ct) = decryptWrapper gpgDecrypt ct
