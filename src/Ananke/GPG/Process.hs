module Ananke.GPG.Process
  ( readProcessWithExitCode,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, handle, mask, onException, throwIO, try)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign.C.Error (Errno (..), ePIPE)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import GHC.IO.Handle (hClose, hSetBinaryMode)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (..))
import System.Process qualified as Process

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle $ \e -> case e of
  IOError {ioe_type = ResourceVanished, ioe_errno = Just ioe} | Errno ioe == ePIPE -> return ()
  _ -> throwIO e

-- | A descendant of ['withForkWait'](https://hackage.haskell.org/package/process/docs/src/System.Process.html#withForkWait).
forkWait :: IO a -> IO (IO a)
forkWait a = do
  mres <- newEmptyMVar :: IO (MVar (Either SomeException a))
  mask $ \restore -> do
    tid <- forkIO $ try (restore a) >>= putMVar mres
    let thunk = takeMVar mres >>= either throwIO return
    return $ onException (restore thunk) (killThread tid)

readProcessWithExitCode :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode cmd args input =
  let cp = (Process.proc cmd args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
   in Process.withCreateProcess cp $ \stdin stdout stderr ph ->
        case (stdin, stdout, stderr) of
          (Just inh, Just outh, Just errh) ->
            do
              hSetBinaryMode inh True
              hSetBinaryMode outh True
              hSetBinaryMode errh True
              outThunk <- forkWait (ByteString.hGetContents outh)
              errThunk <- forkWait (ByteString.hGetContents errh)
              unless (ByteString.null input) (ignoreSIGPIPE (ByteString.hPutStr inh input))
              ignoreSIGPIPE (hClose inh)
              out <- outThunk
              err <- errThunk
              hClose outh
              hClose errh
              ex <- Process.waitForProcess ph
              return (ex, out, err)
          (Nothing, _, _) -> error "readProcessWithExitCode: Failed to get stdin."
          (_, Nothing, _) -> error "readProcessWithExitCode: Failed to get stdout."
          (_, _, Nothing) -> error "readProcessWithExitCode: Failed to get stderr."
