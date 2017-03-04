{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.IO.Config where

import Control.Monad.Except
import Hecate.Types (AppConfig(..), Fingerprint(..))
import Text.Toml
import Text.Toml.Types
import Data.HashMap.Lazy
import qualified Data.Text.IO as TIO

parseFingerprint :: Table -> Either String Fingerprint
parseFingerprint tbl = case tbl ! "recipient" of
                         VTable mm -> case mm ! "fingerprint" of
                           VString mmm -> Right $ Fingerprint mmm
                           _           -> Left "couldn't parse fingerprint (String)"
                         _ -> Left "hecate.toml file does not contain the 'recipient' key"

getAppConfig :: FilePath -> IO AppConfig
getAppConfig file = do
  txt     <- liftIO $ TIO.readFile file
  tbl     <- either (error . show) pure (parseTomlDoc "" txt)
  fp      <- either error pure (parseFingerprint tbl)
  return AppConfig{ appConfigFingerprint = fp }
