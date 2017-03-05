{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hecate.IO where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Digest.Pure.SHA
import Data.Monoid ((<>))
import Data.Time.Clock
import Data.Time.Format
import Data.Text.Encoding
import Hecate.GPG
import Hecate.Types
import System.Directory (doesFileExist)
import System.IO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import qualified Hecate.Database as DB

showTime :: UTCTime -> T.Text
showTime = T.pack . formatTime defaultTimeLocale "%s%Q"

ensureFile :: (MonadIO m, MonadError AppError m) => FilePath -> m FilePath
ensureFile file = do
  exists <- liftIO (doesFileExist file)
  if exists
    then pure file
    else throwError (FileSystem "File does not exist")

ider :: T.Text -> Id
ider = Id . T.pack . showDigest . sha1 . BSL.fromStrict . encodeUtf8

getId :: UTCTime -> Description -> Maybe Identity -> Id
getId ts (Description d) (Just (Identity i)) = ider $ showTime ts <> d <> i
getId ts (Description d) Nothing             = ider $ showTime ts <> d

entry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
entry description identity plaintext meta = do
  timestamp <- liftIO getCurrentTime
  i         <- pure $ getId timestamp description identity
  encrypted <- encryptM plaintext
  return $ Entry i timestamp description identity encrypted meta

getPlainText
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Entry
  -> m Plaintext
getPlainText Entry{..} = decryptM entryCiphertext

withUpdateTimestamp :: MonadIO m => (Entry -> Entry) -> Entry -> m Entry
withUpdateTimestamp f e = f <$> g
  where
    g = (\ts -> e{entryTimestamp = ts}) <$> liftIO getCurrentTime

updateIdentity :: MonadIO m => Maybe Identity -> Entry -> m Entry
updateMetadata :: MonadIO m => Maybe Metadata -> Entry -> m Entry
updateIdentity i = withUpdateTimestamp $ \ue -> ue{entryIdentity = i}
updateMetadata m = withUpdateTimestamp $ \ue -> ue{entryMeta = m}

updateDescription
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Description
  -> Entry
  -> m Entry
updateDescription d e@Entry{entryIdentity, entryMeta} = do
  pt <- getPlainText e
  entry d entryIdentity pt entryMeta

updateCiphertext
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Plaintext
  -> Entry
  -> m Entry
updateCiphertext pt Entry{entryDescription, entryIdentity, entryMeta} =
  entry entryDescription entryIdentity pt entryMeta

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushChar :: Char -> IO ()
flushChar s = putChar s >> hFlush stdout

promptText :: MonadIO m => String -> m String
promptText s = liftIO $ flushStr s >> getLine

makeEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => String
  -> Maybe String
  -> Maybe String
  -> m Entry
makeEntry d i m = do
  t  <- promptText "Enter text to encrypt: "
  entry (Description . T.pack  $  d)
        (Identity    . T.pack <$> i)
        (Plaintext   . T.pack  $  t)
        (Metadata    . T.pack <$> m)

queryFromDescription :: String -> Query
queryFromDescription d =
  Query { queryId          = Nothing
        , queryDescription = Just . Description $ T.pack d
        , queryIdentity    = Nothing
        , queryMeta        = Nothing
        }

entryToDisplayEntry :: Entry -> Plaintext -> DisplayEntry
entryToDisplayEntry Entry{..} p =
  DisplayEntry entryTimestamp entryDescription entryIdentity p entryMeta

decryptEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Entry
  -> m DisplayEntry
decryptEntry e = entryToDisplayEntry e <$> getPlainText e

decryptEntries
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => [Entry]
  -> m [DisplayEntry]
decryptEntries = mapM decryptEntry

inputEntryToEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => InputEntry
  -> m Entry
inputEntryToEntry InputEntry{..} =
  entry inputDescription inputIdentity inputPlaintext inputMeta

importCSV
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => FilePath
  -> m [Entry]
importCSV csvFile = do
  file <- ensureFile csvFile
  bs   <- liftIO $ BSL.readFile file
  ies  <- either (throwError . CsvDecoding) (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM inputEntryToEntry ies

evalCommand
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Command
  -> m Response
evalCommand Add{..}    = do
  ctx <- ask
  e   <- makeEntry addDescription addIdentity addMeta
  _   <- DB.put (_conn ctx) e
  return Added
evalCommand Remove{..} = do
  ctx <- ask
  q   <- pure $ queryFromDescription removeDescription
  es  <- DB.query (_conn ctx) q
  _   <- mapM_ (DB.delete (_conn ctx)) es
  return Removed
evalCommand Lookup{..} = do
  ctx <- ask
  q   <- pure $ queryFromDescription lookupDescription
  res <- DB.query (_conn ctx) q
  case res of
    []  -> MultipleEntries <$> pure []
    [e] -> SingleEntry     <$> decryptEntry e
    es  -> MultipleEntries <$> decryptEntries es
evalCommand Import{importFile} = do
  ctx <- ask
  es  <- importCSV importFile
  _   <- mapM_ (DB.put (_conn ctx)) es
  return Added
