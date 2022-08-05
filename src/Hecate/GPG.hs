{-# LANGUAGE ScopedTypeVariables #-}

module Hecate.GPG
  ( encrypt
  , decrypt
  ) where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (handle, mask, onException, throwIO, try)
import           Control.Monad           (unless)
import           Control.Monad.Catch     (MonadThrow (..), SomeException)
import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Data.ByteString         as BS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as Encoding
import           Foreign.C.Error         (Errno (..), ePIPE)
import           GHC.IO.Exception        (IOErrorType (..), IOException (..))
import           GHC.IO.Handle           (hClose, hSetBinaryMode)
import           System.Exit             (ExitCode (..))
import           System.Process          (CreateProcess (..), StdStream (..))
import qualified System.Process          as Process

import           Hecate.Data             (Ciphertext, KeyId (..), Plaintext (..), mkCiphertext, unCiphertext)
import           Hecate.Error            (AppError (..))


ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle $ \e -> case e of
  IOError{ioe_type = ResourceVanished, ioe_errno = Just ioe} | Errno ioe == ePIPE -> return ()
  _                                                                               -> throwIO e

forkWait :: IO a -> IO (IO a)
forkWait a = do
  mres <- newEmptyMVar :: IO (MVar (Either SomeException a))
  mask $ \restore -> do
    tid <- forkIO $ try (restore a) >>= putMVar mres
    let thunk = takeMVar mres >>= either throwIO return
    return $ onException (restore thunk) (killThread tid)

readProcessWithExitCode :: FilePath -> [String] -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
readProcessWithExitCode cmd args input =
  let cp = (Process.proc cmd args){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  in Process.withCreateProcess cp $ \stdin stdout stderr ph ->
    case (stdin, stdout, stderr)  of
      (Just inh, Just outh, Just errh) -> do
        hSetBinaryMode inh  True
        hSetBinaryMode outh True
        hSetBinaryMode errh True
        outThunk <- forkWait (BS.hGetContents outh)
        errThunk <- forkWait (BS.hGetContents errh)
        unless (BS.null input) (ignoreSIGPIPE (BS.hPutStr inh input))
        ignoreSIGPIPE (hClose inh)
        out <- outThunk
        err <- errThunk
        hClose outh
        hClose errh
        ex <- Process.waitForProcess ph
        return (ex, out, err)
      (Nothing,       _,       _) -> error "readProcessWithExitCode: Failed to get stdin."
      (      _, Nothing,       _) -> error "readProcessWithExitCode: Failed to get stdout."
      (      _,       _, Nothing) -> error "readProcessWithExitCode: Failed to get stderr."

gpgEncrypt :: String -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgDecrypt ::           BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
gpgEncrypt keyid = readProcessWithExitCode "gpg" ["--batch", "-q", "-e", "-r", keyid]
gpgDecrypt       = readProcessWithExitCode "gpg" ["--batch", "-q", "-d"]

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
