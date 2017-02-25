module Hecate.Util where

import Control.Monad.Except
import System.Posix.Env (getEnv)

getHome :: MonadIO m => m (Maybe FilePath)
getHome = liftIO $ getEnv "HOME"

-- | Map over both failure and success.
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
