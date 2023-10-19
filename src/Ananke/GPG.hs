module Ananke.GPG
  ( encrypt
  , decrypt
  ) where

import           Control.Exception  (throwIO)
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as Encoding
import           System.Exit        (ExitCode (..))

import           Ananke.Data        (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import           Ananke.Error       (AppError (..))
import           Ananke.GPG.Process (readProcessWithExitCode)


gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt ::           BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]
gpgDecrypt       = readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

handleGPGExit :: (BS.ByteString -> a) -> IO (ExitCode, BS.ByteString, BS.ByteString) -> IO a
handleGPGExit f ma = do
  (exitCode, stdout, stderr) <- ma
  case exitCode of
    ExitSuccess   -> return $ f stdout
    ExitFailure _ -> throwIO . GPG . T.unpack . Encoding.decodeUtf8 $ stderr

encrypt :: KeyId -> Plaintext -> IO Ciphertext
encrypt (MkKeyId keyid) (MkPlaintext pt) = handleGPGExit mkCiphertext ma
  where
    ma = gpgEncrypt (T.unpack keyid) (Encoding.encodeUtf8 pt)

decrypt :: Ciphertext -> IO Plaintext
decrypt ct = handleGPGExit (MkPlaintext . Encoding.decodeUtf8) ma
  where
    ma = gpgDecrypt (unCiphertext ct)
