{-# LANGUAGE NamedFieldPuns   #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , importCSV
  , exportCSV
  , eval
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Csv                as CSV
import qualified Data.Text               as T
import qualified Data.Vector             as Vector
import           Lens.Family2
import           System.Directory        (doesFileExist)

import           Hecate.Context
import           Hecate.Data             hiding (query)
import qualified Hecate.Data             as Data
import           Hecate.GPG
import           Hecate.Error
import           Hecate.Interfaces


data ModifyAction = Keep | Change
  deriving (Show, Eq)

data Verbosity = Normal | Verbose
  deriving (Show, Eq)

data Target
  = TargetId String
  | TargetDescription String
  deriving (Show, Eq)

-- | 'Command' represents CLI commands
data Command
  = Add { _addDescription :: String
        , _addIdentity    :: Maybe String
        , _addMeta        :: Maybe String
        }
  | Lookup { _lookupDescription :: String
           , _lookupVerbosity   :: Verbosity
           }
  | Import { _importFile :: FilePath }
  | Export { _exportFile :: FilePath }
  | Modify { _modifyTarget     :: Target
           , _modifyCiphertext :: ModifyAction
           , _modifyIdentity   :: Maybe String
           , _modifyMeta       :: Maybe String
           }
  | Redescribe { _redescribeTarget      :: Target
               , _redescribeDescription :: String
               }
  | Remove Target
  | CheckForMultipleKeys
  deriving Show

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry Verbosity
  | MultipleEntries [DisplayEntry] Verbosity
  | Added
  | Exported
  | Modified
  | Redescribed
  | Removed
  | CheckedForMultipleKeys
  deriving (Show, Eq)

ensureFile :: MonadIO m => FilePath -> m FilePath
ensureFile file = do
  exists <- liftIO (doesFileExist file)
  if exists
    then return file
    else liftIO (throwIO (FileSystem "File does not exist"))

checkKey
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => m Response
  -> m Response
checkKey k = do
  ctx <- ask
  let keyId         = ctx ^. configKeyId
      allowMultKeys = ctx ^. configAllowMultipleKeys
  total <- getCount
  if total == 0
    then k
    else do
    let q     = "New keyid found: do you want to re-encrypt all entries?"
        reenc = reencryptAll keyId
        err   = liftIO (throwIO (Default "You have set allow_multiple_keys to false"))
    count <- getCountOfKeyId keyId
    case (count, allowMultKeys) of
      (0, False)              -> binaryChoice q (reenc >> k) err
      (x, _    ) | x == total -> k
      (_, True )              -> binaryChoice q (reenc >> k) k
      (_, False)              -> liftIO (throwIO (Default "All entries do not have the same keyid"))

csvEntryToEntry
  :: (MonadIO m, MonadInteraction m, MonadReader r m, HasConfig r)
  => CSVEntry
  -> m Entry
csvEntryToEntry entry = do
  ctx       <- ask
  timestamp <- now
  createEntry (ctx ^. configKeyId)
              timestamp
              (_csvDescription entry)
              (_csvIdentity entry)
              (_csvPlaintext entry)
              (_csvMeta entry)

importCSV
  :: (MonadIO m, MonadInteraction m, MonadReader r m, HasConfig r)
  => FilePath
  -> m [Entry]
importCSV csvFile = do
  file <- ensureFile csvFile
  bs   <- liftIO (BSL.readFile file)
  ies  <- either (liftIO . throwIO . CsvDecoding) (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM csvEntryToEntry ies

exportCSV
  :: MonadIO m
  => FilePath
  -> [Entry]
  -> m ()
exportCSV csvFile entries = do
  csvEntries <- mapM entryToCSVEntry entries
  csv        <- pure (CSV.encode csvEntries)
  liftIO (BSL.writeFile csvFile csv)

createEntryWrapper
  :: (MonadIO m, MonadInteraction m, MonadReader r m, HasConfig r)
  => String
  -> Maybe String
  -> Maybe String
  -> String
  -> m Entry
createEntryWrapper d i m t = do
  ctx       <- ask
  timestamp <- now
  createEntry (ctx ^. configKeyId)
              timestamp
              (Description . T.pack  $  d)
              (Identity    . T.pack <$> i)
              (Plaintext   . T.pack  $  t)
              (Metadata    . T.pack <$> m)

updateWrapper
  :: MonadIO m
  => Maybe String
  -> Maybe String
  -> Entry
  -> m Entry
updateWrapper Nothing Nothing e =
  return e
updateWrapper (Just "") Nothing e =
  updateIdentity Nothing e
updateWrapper miden Nothing e =
  updateIdentity (Identity . T.pack <$> miden) e
updateWrapper Nothing (Just "") e =
  updateMetadata Nothing e
updateWrapper Nothing mmeta e =
  updateMetadata (Metadata . T.pack <$> mmeta) e
updateWrapper (Just "") (Just "") e =
  updateIdentity Nothing e >>= updateMetadata Nothing
updateWrapper miden mmeta e =
  updateIdentity (Identity . T.pack <$> miden) e >>=
  updateMetadata (Metadata . T.pack <$> mmeta)

updateCiphertext
  :: (MonadIO m, MonadInteraction m, MonadReader r m, HasConfig r)
  => Plaintext
  -> Entry
  -> m Entry
updateCiphertext plaintext entry = do
  ctx       <- ask
  timestamp <- now
  createEntry (ctx ^. configKeyId)
              timestamp
              (_entryDescription entry)
              (_entryIdentity entry)
              plaintext
              (_entryMeta entry)

updateCiphertextWrapper
  :: (MonadIO m, MonadInteraction m, MonadReader r m, HasConfig r)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertextWrapper Change e =
  prompt "Enter text to encrypt: " >>= \ t -> updateCiphertext (Plaintext . T.pack $ t) e
updateCiphertextWrapper Keep e =
  return e

modifyOnlySingletons
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
modifyOnlySingletons [e] maction miden mmeta = do
  ue1 <- updateWrapper miden mmeta e
  ue2 <- updateCiphertextWrapper maction ue1
  _   <- put ue2
  _   <- delete e
  return Modified
modifyOnlySingletons _ _ _ _ =
  liftIO (throwIO (AmbiguousInput "There are multiple entries matching your input criteria."))

findAndModify
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => Query
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
findAndModify q maction miden mmeta
  = checkKey k
  where
    k = do
      rs  <- query q
      modifyOnlySingletons rs maction miden mmeta

modify
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => Target
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
modify (TargetId mid) maction miden mmeta =
  findAndModify (Data.query (Just mid) Nothing Nothing Nothing) maction miden mmeta
modify (TargetDescription mdesc) maction miden mmeta =
  findAndModify (Data.query Nothing (Just mdesc) Nothing Nothing) maction miden mmeta

redescribeOnlySingletons
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> String
  -> m Response
redescribeOnlySingletons [e] s
  = checkKey k
  where
    k = do
      ue  <- updateDescription (Description . T.pack $ s) e
      _   <- put ue
      _   <- delete e
      return Redescribed
redescribeOnlySingletons _ _ =
  liftIO (throwIO (AmbiguousInput "There are multiple entries matching your input criteria."))

findAndRedescribe
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => Query
  -> String
  -> m Response
findAndRedescribe q s = do
  rs  <- query q
  redescribeOnlySingletons rs s

redescribe
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => Target
  -> String
  -> m Response
redescribe (TargetId tid) s =
  findAndRedescribe (Data.query (Just tid) Nothing Nothing Nothing) s
redescribe (TargetDescription tdesc) s =
  findAndRedescribe (Data.query Nothing (Just tdesc) Nothing Nothing) s

removeOnlySingletons
  :: (MonadIO m, MonadStore m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> m Response
removeOnlySingletons [e] =
  delete e >> return Removed
removeOnlySingletons _ =
  liftIO (throwIO (AmbiguousInput "There are multiple entries matching your input criteria."))

findAndRemove
  :: (MonadIO m, MonadStore m, MonadReader r m, HasAppContext r)
  => Query
  -> m Response
findAndRemove q =
  query q >>= removeOnlySingletons

remove
  :: (MonadIO m, MonadStore m, MonadReader r m, HasAppContext r)
  => Target
  -> m Response
remove (TargetId rid) =
  findAndRemove (Data.query (Just rid) Nothing Nothing Nothing)
remove (TargetDescription rdesc) =
  findAndRemove (Data.query Nothing (Just rdesc) Nothing Nothing)

check
  :: (MonadIO m, MonadStore m, MonadReader r m, HasAppContext r)
  => m Response
check = do
  ctx <- ask
  t   <- getCount
  let keyId = ctx ^. configKeyId
  r <- getCountOfKeyId keyId
  if t == r
    then return CheckedForMultipleKeys
    else liftIO (throwIO (Default "All entries do not have the same keyid"))

eval
  :: (MonadIO m, MonadInteraction m, MonadStore m, MonadReader r m, HasAppContext r)
  => Command
  -> m Response
eval Add{_addDescription, _addIdentity, _addMeta} =
  checkKey k
  where
    k = prompt "Enter text to encrypt: "                         >>=
        createEntryWrapper _addDescription _addIdentity _addMeta >>=
        put                                                      >>
        return Added
eval Lookup{_lookupDescription, _lookupVerbosity} = do
  q   <- pure (Data.query Nothing (Just _lookupDescription) Nothing Nothing)
  res <- query q
  case res of
    []  -> MultipleEntries <$> pure []                     <*> pure _lookupVerbosity
    [e] -> SingleEntry     <$> entryToDisplayEntry e       <*> pure _lookupVerbosity
    es  -> MultipleEntries <$> mapM entryToDisplayEntry es <*> pure _lookupVerbosity
eval Import{_importFile} =
  checkKey k
  where
    k = importCSV _importFile >>= mapM_ put >> return Added
eval Export{_exportFile}  = do
  es  <- selectAll
  _   <- exportCSV _exportFile es
  return Exported
eval (Modify t c i m)     = modify t c i m
eval (Redescribe t s)     = redescribe t s
eval (Remove t)           = remove t
eval CheckForMultipleKeys = check
