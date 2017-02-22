{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module Hecate.IO where

import Control.Monad
import Control.Monad.Except
import Data.Time.Clock (UTCTime, getCurrentTime)
import Hecate.Crypto
import Hecate.Types
import System.Directory (createDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.Posix.Env (getEnv)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

parseAuth :: MonadError Error m => BSL.ByteString -> m Auth
parseAuth bs = either (throwError . JsonDecoding) pure (Aeson.eitherDecode bs)

getAuth :: MonadError Error m => MasterPassword -> BSL.ByteString -> m MasterKey
getAuth mp bs = parseAuth bs >>= ensureAuth mp

readAuthFile :: MonadIO m => FilePath -> m BSL.ByteString
readAuthFile path = liftIO $ BSL.readFile path

writeAuthFile :: MonadIO m => FilePath -> Auth -> m ()
writeAuthFile path a = liftIO $ BSL.writeFile path (Aeson.encode a)

getHome :: (MonadIO m) => m (Maybe FilePath)
getHome = liftIO $ getEnv "HOME"

loadAuth
  :: (MonadIO m, MonadError Error m)
  => MasterPassword
  -> FilePath
  -> m MasterKey
loadAuth mp authFile = do
  let path = takeDirectory authFile
  dirExists  <- liftIO $ doesDirectoryExist path
  unless dirExists (liftIO $ createDirectory path)
  fileExists <- liftIO $ doesFileExist authFile
  if fileExists
    then readAuthFile authFile  >>= getAuth mp
    else makeSalt 24            >>=
         (pure . genAuth mp)    >>=
         writeAuthFile authFile >>
         readAuthFile authFile  >>= getAuth mp

entry
  :: (MonadIO m, MonadError Error m)
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
  :: (MonadIO m, MonadError Error m)
  => MasterKey
  -> Entry
  -> m PlainText
getCipherText mk e = do
  (decrypted, tag) <- decryptM mk (nonce e) (description e) (cipherText e)
  if tag == authTag e
    then return decrypted
    else throwError (Integrity "Tags do not match")

withUpdateTimestamp :: MonadIO m => (UTCTime -> m Entry) -> m Entry
withUpdateTimestamp f = liftIO getCurrentTime >>= f

updateDescription :: MonadIO m => Entry -> Description -> m Entry
updateDescription e d = withUpdateTimestamp $ \ts ->
  pure (e {description = d, timestamp = ts})

updateIdentity :: MonadIO m => Entry -> Maybe Identity -> m Entry
updateIdentity e u = withUpdateTimestamp $ \ts ->
  pure (e {username = u, timestamp = ts})

updateCipherText :: MonadIO m => Entry -> CipherText -> m Entry
updateCipherText e p = withUpdateTimestamp $ \ts ->
  pure (e {cipherText = p, timestamp = ts})

updateMetadata :: MonadIO m => Entry -> Maybe Metadata -> m Entry
updateMetadata e n = withUpdateTimestamp $ \ts ->
  pure (e {meta = n, timestamp = ts})
