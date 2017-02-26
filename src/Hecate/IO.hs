{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hecate.IO where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Time.Clock (getCurrentTime)
import Hecate.Crypto
import Hecate.Types
import System.Directory (createDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.IO
import System.Posix.Env (getEnv)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Hecate.Database as DB

getHome :: MonadIO m => m (Maybe FilePath)
getHome = liftIO $ getEnv "HOME"

genAuth :: MasterPassword -> Salt -> Auth
genAuth mp s = Auth { key = generateMasterKey mp s, salt = s }

parseAuth :: MonadError AppError m => BSL.ByteString -> m Auth
parseAuth = either (throwError . JsonDecoding) pure . Aeson.eitherDecode

getAuth :: MonadError AppError m => MasterPassword -> BSL.ByteString -> m MasterKey
getAuth mp bs = parseAuth bs >>= ensureAuth mp

readAuthFile :: MonadIO m => FilePath -> m BSL.ByteString
readAuthFile = liftIO . BSL.readFile

writeAuthFile :: MonadIO m => FilePath -> Auth -> m ()
writeAuthFile path = liftIO . BSL.writeFile path . Aeson.encode

ensureAuth :: MonadError AppError m => MasterPassword -> Auth -> m MasterKey
ensureAuth mp a =
  if key a == generateMasterKey mp (salt a)
  then pure (key a)
  else throwError (AuthVerification "Can't re-generate MasterKey from given MasterPassword")

loadAuth
  :: (MonadIO m, MonadError AppError m)
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
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
entry mk description username plaintext meta = do
  nonce            <- makeNonce
  timestamp        <- liftIO getCurrentTime
  (encrypted, tag) <- encryptM mk nonce description plaintext
  return $ Entry nonce tag timestamp description username encrypted meta

getPlainText
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Entry
  -> m Plaintext
getPlainText mk e = do
  (decrypted, tag) <- decryptM mk (nonce e) (description e) (ciphertext e)
  if tag == authTag e
    then return decrypted
    else throwError (Integrity "Tags do not match")

withUpdateTimestamp :: MonadIO m => (Entry -> Entry) -> Entry -> m Entry
withUpdateTimestamp f e = f <$> g
  where
    g = (\ts -> e {timestamp = ts}) <$> liftIO getCurrentTime

updateIdentity :: MonadIO m => Maybe Identity -> Entry -> m Entry
updateMetadata :: MonadIO m => Maybe Metadata -> Entry -> m Entry
updateIdentity i = withUpdateTimestamp $ \ue -> ue {identity = i}
updateMetadata m = withUpdateTimestamp $ \ue -> ue {meta = m}

updateDescription
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Description
  -> Entry
  -> m Entry
updateDescription mk d e = do
  pt <- getPlainText mk e
  entry mk d (identity e) pt (meta e)

updateCiphertext
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Plaintext
  -> Entry
  -> m Entry
updateCiphertext mk pt e =
  entry mk (description e) (identity e) pt (meta e)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushChar :: Char -> IO ()
flushChar s = putChar s >> hFlush stdout

promptText :: MonadIO m => String -> m String
promptText s = liftIO $ flushStr s >> getLine

promptTextHidden :: MonadIO m => String -> m String
promptTextHidden s = liftIO $ do
  _ <- flushStr s
  _ <- hSetEcho stdin False
  t <- getLine
  _ <- hSetEcho stdin True
  _ <- flushChar '\n'
  return t

authorize :: (MonadIO m, MonadError AppError m) => FilePath -> m MasterKey
authorize fp = promptTextHidden p >>= flip loadAuth fp . MasterPassword . T.pack
  where
    p = "Enter your master password: "

makeEntry
  :: (MonadIO m, MonadError AppError m)
  => FilePath
  -> String
  -> Maybe String
  -> Maybe String
  -> m Entry
makeEntry authFile d i m = do
  mk <- authorize authFile
  t  <- promptText "Enter text to encrypt: "
  entry mk (Description . T.pack  $  d)
           (Identity    . T.pack <$> i)
           (Plaintext   . T.pack  $  t)
           (Metadata    . T.pack <$> m)

queryFromDescription :: String -> Query
queryFromDescription d =
  Query { queryNonce       = Nothing
        , queryDescription = Just . Description $ T.pack d
        , queryIdentity    = Nothing
        , queryMeta        = Nothing
        }

verifiedQuery
  :: (MonadIO m, MonadError AppError m)
  => FilePath
  -> String
  -> m Query
verifiedQuery authFile d = authorize authFile >> pure (queryFromDescription d)

entryToDisplayEntry :: Entry -> Plaintext -> DisplayEntry
entryToDisplayEntry Entry {timestamp, description, identity, meta} p =
  DisplayEntry timestamp description identity p meta

getDisplayEntry
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Entry
  -> m DisplayEntry
getDisplayEntry mk e = entryToDisplayEntry e <$> getPlainText mk e

decryptEntry
  :: (MonadIO m, MonadError AppError m)
  => FilePath
  -> Entry
  -> m DisplayEntry
decryptEntry authFile e = do
  mk <- authorize authFile
  getDisplayEntry mk e

decryptEntries
  :: (MonadIO m, MonadError AppError m)
  => FilePath
  -> [Entry]
  -> m [DisplayEntry]
decryptEntries authFile es = do
  mk <- authorize authFile
  mapM (getDisplayEntry mk) es

evalCommand
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Command
  -> m Response
evalCommand a@Add{}    = do
  ctx <- ask
  e   <- makeEntry (_authFile ctx) (addDescription a) (addIdentity a) (addMeta a)
  _   <- DB.put (_conn ctx) e
  return Added
evalCommand r@Remove{} = do
  ctx <- ask
  q   <- verifiedQuery (_authFile ctx) (removeDescription r)
  es  <- DB.query (_conn ctx) q
  _   <- mapM_ (DB.delete (_conn ctx)) es
  return Removed
evalCommand l@Lookup{} = do
  ctx <- ask
  q   <- pure $ queryFromDescription (lookupDescription l)
  es  <- DB.query (_conn ctx) q
  MultipleEntries <$> decryptEntries (_authFile ctx) es
