module Ananke.GPG
  ( encrypt,
    decrypt,
  )
where

import Ananke.Data (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import Ananke.Error (AppError (..))
import Ananke.GPG.Process (readProcessWithExitCode)
import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import System.Exit (ExitCode (..))

gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]

gpgDecrypt :: BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt = readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

handleGPGExit :: (BS.ByteString -> a) -> IO (ExitCode, BS.ByteString, BS.ByteString) -> IO a
handleGPGExit f ma = do
  (exitCode, stdout, stderr) <- ma
  case exitCode of
    ExitSuccess -> return $ f stdout
    ExitFailure _ -> throwIO . GPG . Text.unpack . Encoding.decodeUtf8 $ stderr

encrypt :: KeyId -> Plaintext -> IO Ciphertext
encrypt (MkKeyId keyid) (MkPlaintext pt) = handleGPGExit mkCiphertext ma
  where
    ma = gpgEncrypt (Text.unpack keyid) (Encoding.encodeUtf8 pt)

decrypt :: Ciphertext -> IO Plaintext
decrypt ct = handleGPGExit (MkPlaintext . Encoding.decodeUtf8) ma
  where
    ma = gpgDecrypt (unCiphertext ct)
