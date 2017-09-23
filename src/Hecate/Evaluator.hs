{-# LANGUAGE NamedFieldPuns   #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , eval
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Csv               as CSV
import qualified Data.Text              as T
import qualified Data.Vector            as Vector
import           Lens.Simple            hiding (Identity)
import           System.Directory       (doesFileExist)
import           System.IO              (hFlush, stdout)

import           Hecate.Context
import qualified Hecate.Database        as DB
import           Hecate.Data
import           Hecate.GPG
import           Hecate.Error


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
  | Modified
  | Redescribed
  | Removed
  | CheckedForMultipleKeys
  deriving (Show, Eq)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

promptText :: MonadIO m => String -> m String
promptText s = liftIO (flushStr s >> getLine)

ensureFile :: MonadIO m => FilePath -> m FilePath
ensureFile file = do
  exists <- liftIO (doesFileExist file)
  if exists
    then return file
    else throw (FileSystem "File does not exist")

importCSV
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => FilePath
  -> m [Entry]
importCSV csvFile = do
  file <- ensureFile csvFile
  bs   <- liftIO (BSL.readFile file)
  ies  <- either (throw . CsvDecoding) (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM importEntryToEntry ies

createEntryWrapper
  :: (MonadIO m, MonadReader r m, HasAppContext r)
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

updateCiphertextWrapper
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertextWrapper Change e =
  promptText "Enter text to encrypt: " >>= \ t -> updateCiphertext (Plaintext . T.pack $ t) e
updateCiphertextWrapper Keep e =
  return e

modifyOnlySingletons
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
modifyOnlySingletons [e] maction miden mmeta = do
  ctx <- ask
  ue1 <- updateWrapper miden mmeta e
  ue2 <- updateCiphertextWrapper maction ue1
  _   <- DB.put    (ctx ^. appContextConnection) ue2
  _   <- DB.delete (ctx ^. appContextConnection) e
  return Modified
modifyOnlySingletons _ _ _ _ =
  throw (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndModify
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Query
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
findAndModify q maction miden mmeta = do
  ctx <- ask
  rs  <- DB.query (ctx ^. appContextConnection) q
  modifyOnlySingletons rs maction miden mmeta

modify
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Target
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
modify (TargetId mid) maction miden mmeta =
  findAndModify (query (Just mid) Nothing Nothing Nothing) maction miden mmeta
modify (TargetDescription mdesc) maction miden mmeta =
  findAndModify (query Nothing (Just mdesc) Nothing Nothing) maction miden mmeta

redescribeOnlySingletons
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> String
  -> m Response
redescribeOnlySingletons [e] s = do
  ctx <- ask
  ue  <- updateDescription (Description . T.pack $ s) e
  _   <- DB.put    (ctx ^. appContextConnection) ue
  _   <- DB.delete (ctx ^. appContextConnection) e
  return Redescribed
redescribeOnlySingletons _ _ =
  throw (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndRedescribe
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Query
  -> String
  -> m Response
findAndRedescribe q s = do
  ctx <- ask
  rs  <- DB.query (ctx ^. appContextConnection) q
  redescribeOnlySingletons rs s

redescribe
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Target
  -> String
  -> m Response
redescribe (TargetId tid) s =
  findAndRedescribe (query (Just tid) Nothing Nothing Nothing) s
redescribe (TargetDescription tdesc) s =
  findAndRedescribe (query Nothing (Just tdesc) Nothing Nothing) s

removeOnlySingletons
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => [Entry]
  -> m Response
removeOnlySingletons [e] = do
  ctx <- ask
  _   <- DB.delete (ctx ^. appContextConnection) e
  return Removed
removeOnlySingletons _ =
  throw (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndRemove
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Query
  -> m Response
findAndRemove q = do
  ctx <- ask
  rs  <- DB.query (ctx ^. appContextConnection) q
  removeOnlySingletons rs

remove
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Target
  -> m Response
remove (TargetId rid) =
  findAndRemove (query (Just rid) Nothing Nothing Nothing)
remove (TargetDescription rdesc) =
  findAndRemove (query Nothing (Just rdesc) Nothing Nothing)

check
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => m Response
check = do
  ctx <- ask
  r   <- DB.checkEntries (ctx ^. appContextConnection) (ctx ^. appContextKeyId)
  if r
    then return CheckedForMultipleKeys
    else throw (Default "All entries do not have the same keyid")

eval
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Command
  -> m Response
eval Add{_addDescription, _addIdentity, _addMeta} = do
  ctx <- ask
  t   <- promptText "Enter text to encrypt: "
  e   <- createEntryWrapper _addDescription _addIdentity _addMeta t
  _   <- DB.put (ctx ^. appContextConnection) e
  return Added
eval Lookup{_lookupDescription, _lookupVerbosity} = do
  ctx <- ask
  q   <- pure (query Nothing (Just _lookupDescription) Nothing Nothing)
  res <- DB.query (ctx ^. appContextConnection) q
  case res of
    []  -> MultipleEntries <$> pure []           <*> pure _lookupVerbosity
    [e] -> SingleEntry     <$> decryptEntry e    <*> pure _lookupVerbosity
    es  -> MultipleEntries <$> decryptEntries es <*> pure _lookupVerbosity
eval Import{_importFile} = do
  ctx <- ask
  es  <- importCSV _importFile
  _   <- mapM_ (DB.put (ctx ^. appContextConnection)) es
  return Added
eval (Modify t c i m)     = modify t c i m
eval (Redescribe t s)     = redescribe t s
eval (Remove t)           = remove t
eval CheckForMultipleKeys = check
