{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Hecate.Evaluator
  ( Verbosity(..)
  , Removal(..)
  , Command(..)
  , Response(..)
  , eval
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Hecate.Database as DB

import Hecate.Context
import Hecate.Data
import Hecate.GPG
import Hecate.Error


data Verbosity = Normal | Verbose
  deriving (Show, Eq)

data Removal
  = RemoveId String
  | RemoveDescription String
  deriving (Show, Eq)

-- | 'Command' represents CLI commands
data Command
  = Add { addDescription :: String
        , addIdentity    :: Maybe String
        , addMeta        :: Maybe String
        }
  | Lookup { lookupDescription :: String
           , verbosity         :: Verbosity
           }
  | Import { importFile :: FilePath }
  | Remove Removal
  deriving Show

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry Verbosity
  | MultipleEntries [DisplayEntry] Verbosity
  | Added
  | Removed
  deriving (Show, Eq)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

promptText :: MonadIO m => String -> m String
promptText s = liftIO $ flushStr s >> getLine

ensureFile :: (MonadIO m, MonadError AppError m) => FilePath -> m FilePath
ensureFile file = do
  exists <- liftIO (doesFileExist file)
  if exists
    then pure file
    else throwError (FileSystem "File does not exist")

importCSV
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => FilePath
  -> m [Entry]
importCSV csvFile = do
  file <- ensureFile csvFile
  bs   <- liftIO $ BSL.readFile file
  ies  <- either (throwError . CsvDecoding) (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM importEntryToEntry ies

createEntryWrapper
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => String
  -> Maybe String
  -> Maybe String
  -> String
  -> m Entry
createEntryWrapper d i m t =
  createEntry (Description . T.pack  $  d)
              (Identity    . T.pack <$> i)
              (Plaintext   . T.pack  $  t)
              (Metadata    . T.pack <$> m)

removeOnlySingletons :: (MonadIO m, MonadError AppError m) => AppContext -> [Entry] -> m ()
removeOnlySingletons ctx [e] = DB.delete (appContextConnection ctx) e
removeOnlySingletons _   _   = throwError (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndRemove :: (MonadIO m, MonadError AppError m) => AppContext -> Query -> m Response
findAndRemove ctx q = DB.query (appContextConnection ctx) q >>= removeOnlySingletons ctx >> return Removed

remove :: (MonadIO m, MonadError AppError m) => Removal -> AppContext -> m Response
remove (RemoveId rid)            ctx = findAndRemove ctx (query (Just rid) Nothing      Nothing Nothing)
remove (RemoveDescription rdesc) ctx = findAndRemove ctx (query Nothing    (Just rdesc) Nothing Nothing)

eval
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Command
  -> m Response
eval Add{..}    = do
  ctx <- ask
  t   <- promptText "Enter text to encrypt: "
  e   <- createEntryWrapper addDescription addIdentity addMeta t
  _   <- DB.put (appContextConnection ctx) e
  return Added
eval Lookup{..} = do
  ctx <- ask
  q   <- pure $ queryFromDescription lookupDescription
  res <- DB.query (appContextConnection ctx) q
  case res of
    []  -> MultipleEntries <$> pure []           <*> pure verbosity
    [e] -> SingleEntry     <$> decryptEntry e    <*> pure verbosity
    es  -> MultipleEntries <$> decryptEntries es <*> pure verbosity
eval Import{importFile} = do
  ctx <- ask
  es  <- importCSV importFile
  _   <- mapM_ (DB.put (appContextConnection ctx)) es
  return Added
eval (Remove r) = ask >>= remove r
