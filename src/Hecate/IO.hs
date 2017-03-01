{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hecate.IO where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Random.Entropy (getEntropy)
import Data.Time.Clock (getCurrentTime)
import Hecate.Crypto
import Hecate.Types
import System.Directory (doesFileExist)
import System.IO
import System.Posix.Env (getEnv)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import qualified Hecate.Database as DB

getHome :: MonadIO m => m (Maybe FilePath)
getHome = liftIO $ getEnv "HOME"

makeValidator :: (MonadIO m, MonadError AppError m) => MasterKey -> m Validator
makeValidator mk = do
  rNonce              <- makeNonce
  rDesc               <- Description . toBase64 <$> liftIO (getEntropy 24)
  rPlaintext          <- Plaintext   . toBase64 <$> liftIO (getEntropy 24)
  (rCiphertext, rTag) <- encryptM mk rNonce rDesc rPlaintext
  return $ Validator rNonce rTag rDesc rPlaintext rCiphertext

validate :: (MonadIO m, MonadError AppError m) => MasterKey -> Validator -> m Bool
validate mk (Validator rNonce rTag rDesc rPlaintext rCiphertext) = do
  (pt, tag) <- decryptM mk rNonce rDesc rCiphertext
  return (tag == rTag && pt == rPlaintext)

genAuth :: (MonadIO m, MonadError AppError m) => MasterKey -> Salt -> m Auth
genAuth mk s = do
  v <- makeValidator mk
  return Auth { validator = v, salt = s }

parseAuth :: MonadError AppError m => BSL.ByteString -> m Auth
parseAuth = either (throwError . JsonDecoding) pure . Aeson.eitherDecode

getAuth
  :: (MonadIO m, MonadError AppError m)
  => MasterPassword
  -> BSL.ByteString
  -> m MasterKey
getAuth mp bs = parseAuth bs >>= ensureAuth mp

readAuthFile :: MonadIO m => FilePath -> m BSL.ByteString
readAuthFile = liftIO . BSL.readFile

writeAuthFile :: MonadIO m => FilePath -> Auth -> m ()
writeAuthFile path = liftIO . BSL.writeFile path . AesonPretty.encodePretty

ensureAuth
  :: (MonadIO m, MonadError AppError m)
  => MasterPassword
  -> Auth
  -> m MasterKey
ensureAuth mp Auth{..} = do
  mk <- pure $ generateMasterKey mp salt
  b  <- validate mk validator
  if b
    then pure mk
    else throwError (AuthVerification "Can't re-generate MasterKey from given MasterPassword")

ensureFile :: (MonadIO m, MonadError AppError m) => FilePath -> m FilePath
ensureFile file = do
  exists <- liftIO (doesFileExist file)
  if exists
    then pure file
    else throwError (FileSystem "File does not exist")

loadAuth
  :: (MonadIO m, MonadError AppError m)
  => MasterPassword
  -> FilePath
  -> m MasterKey
loadAuth mp authFile = do
  fileExists <- liftIO $ doesFileExist authFile
  if fileExists
    then readAuthFile authFile >>= getAuth mp
    else do
       salt <- makeSalt 24
       mk   <- pure $ generateMasterKey mp salt
       auth <- genAuth mk salt
       _    <- writeAuthFile authFile auth
       file <- readAuthFile authFile
       getAuth mp file

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
getPlainText mk Entry{entryNonce, entryAuthTag, entryDescription, entryCiphertext} = do
  (decrypted, tag) <- decryptM mk entryNonce entryDescription entryCiphertext
  if tag == entryAuthTag
    then return decrypted
    else throwError (Integrity "Tags do not match")

withUpdateTimestamp :: MonadIO m => (Entry -> Entry) -> Entry -> m Entry
withUpdateTimestamp f e = f <$> g
  where
    g = (\ts -> e{entryTimestamp = ts}) <$> liftIO getCurrentTime

updateIdentity :: MonadIO m => Maybe Identity -> Entry -> m Entry
updateMetadata :: MonadIO m => Maybe Metadata -> Entry -> m Entry
updateIdentity i = withUpdateTimestamp $ \ue -> ue{entryIdentity = i}
updateMetadata m = withUpdateTimestamp $ \ue -> ue{entryMeta = m}

updateDescription
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Description
  -> Entry
  -> m Entry
updateDescription mk d e@Entry{entryIdentity, entryMeta} = do
  pt <- getPlainText mk e
  entry mk d entryIdentity pt entryMeta

updateCiphertext
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Plaintext
  -> Entry
  -> m Entry
updateCiphertext mk pt Entry{entryDescription, entryIdentity, entryMeta} =
  entry mk entryDescription entryIdentity pt entryMeta

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
entryToDisplayEntry Entry{..} p =
  DisplayEntry entryTimestamp entryDescription entryIdentity p entryMeta

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

inputEntryToEntry
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> InputEntry
  -> m Entry
inputEntryToEntry mk InputEntry{..} =
  entry mk inputDescription inputIdentity inputPlaintext inputMeta

importCSV :: (MonadIO m, MonadError AppError m) => FilePath -> FilePath -> m [Entry]
importCSV authFile csvFile = do
  mk   <- authorize authFile
  file <- ensureFile csvFile
  bs   <- liftIO $ BSL.readFile file
  ies  <- either (throwError . CsvDecoding) (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM (inputEntryToEntry mk) ies

evalCommand
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Command
  -> m Response
evalCommand Add{..}    = do
  ctx <- ask
  e   <- makeEntry (_authFile ctx) addDescription addIdentity addMeta
  _   <- DB.put (_conn ctx) e
  return Added
evalCommand Remove{..} = do
  ctx <- ask
  q   <- verifiedQuery (_authFile ctx) removeDescription
  es  <- DB.query (_conn ctx) q
  _   <- mapM_ (DB.delete (_conn ctx)) es
  return Removed
evalCommand Lookup{..} = do
  ctx <- ask
  q   <- pure $ queryFromDescription lookupDescription
  es  <- DB.query (_conn ctx) q
  MultipleEntries <$> decryptEntries (_authFile ctx) es
evalCommand Import{importFile} = do
  ctx <- ask
  es  <- importCSV (_authFile ctx) importFile
  _   <- mapM_ (DB.put (_conn ctx)) es
  return Added
