{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Client where

import Control.Monad.Except
import Data.Time.Clock (getCurrentTime)
import Hecate.Client.Crypto
import Hecate.Client.Types
import Hecate.Types
import System.Directory (createDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeDirectory)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

parseAuth :: MonadError ClientError m => BSL.ByteString -> m Auth
parseAuth = either (throwError . JsonDecoding) pure . Aeson.eitherDecode

getAuth :: MonadError ClientError m => MasterPassword -> BSL.ByteString -> m MasterKey
getAuth mp bs = parseAuth bs >>= ensureAuth mp

readAuthFile :: MonadIO m => FilePath -> m BSL.ByteString
readAuthFile = liftIO . BSL.readFile

writeAuthFile :: MonadIO m => FilePath -> Auth -> m ()
writeAuthFile path = liftIO . BSL.writeFile path . Aeson.encode

loadAuth
  :: (MonadIO m, MonadError ClientError m)
  => MasterPassword
  -> FilePath
  -> m MasterKey
loadAuth mp authFile = do
  let path = takeDirectory authFile
  dirExists  <- liftIO $ doesDirectoryExist path
  unless dirExists (liftIO $ createDirectory path)
  fileExists <- liftIO $ doesFileExist authFile
  if fileExists
    then readAuthFile authFile      >>= getAuth mp
    else genAuth mp <$> makeSalt 24 >>=
         writeAuthFile authFile     >>
         readAuthFile authFile      >>= getAuth mp

entry
  :: (MonadIO m, MonadError ClientError m)
  => MasterKey
  -> Description
  -> Maybe Identity
  -> PlainText
  -> Maybe Metadata
  -> m Entry
entry mk description username password meta = do
  nonce            <- makeNonce
  timestamp        <- liftIO getCurrentTime
  (encrypted, tag) <- encryptM mk nonce description password
  return $ Entry nonce tag timestamp description username encrypted meta

getCipherText
  :: (MonadIO m, MonadError ClientError m)
  => MasterKey
  -> Entry
  -> m PlainText
getCipherText mk e = do
  (decrypted, tag) <- decryptM mk (nonce e) (description e) (cipherText e)
  if tag == authTag e
    then return decrypted
    else throwError (Integrity "Tags do not match")

withUpdateTimestamp :: MonadIO m => (Entry -> Entry) -> Entry -> m Entry
withUpdateTimestamp f e = f <$> g
  where
    g = (\ts -> e {timestamp = ts}) <$> liftIO getCurrentTime

updateDescription :: MonadIO m => Description    -> Entry -> m Entry
updateIdentity    :: MonadIO m => Maybe Identity -> Entry -> m Entry
updateCipherText  :: MonadIO m => CipherText     -> Entry -> m Entry
updateMetadata    :: MonadIO m => Maybe Metadata -> Entry -> m Entry

updateDescription d = withUpdateTimestamp $ \ue -> ue {description = d}
updateIdentity i    = withUpdateTimestamp $ \ue -> ue {identity = i}
updateCipherText ct = withUpdateTimestamp $ \ue -> ue {cipherText = ct}
updateMetadata m    = withUpdateTimestamp $ \ue -> ue {meta = m}
