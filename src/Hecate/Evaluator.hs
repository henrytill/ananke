{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , eval
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv             as CSV
import qualified Data.Text            as T
import qualified Data.Vector          as Vector
import           System.Directory     (doesFileExist)
import           System.IO            (hFlush, stdout)

import           Hecate.Context
import qualified Hecate.Database      as DB
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
  = Add { addDescription :: String
        , addIdentity    :: Maybe String
        , addMeta        :: Maybe String
        }
  | Lookup { lookupDescription :: String
           , lookupVerbosity   :: Verbosity
           }
  | Import { importFile :: FilePath }
  | Modify { modifyTarget     :: Target
           , modifyCiphertext :: ModifyAction
           , modifyIdentity   :: Maybe String
           , modifyMeta       :: Maybe String
           }
  | Redescribe { redescribeTarget      :: Target
               , redescribeDescription :: String
               }
  | Remove Target
  | Check
  deriving Show

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry Verbosity
  | MultipleEntries [DisplayEntry] Verbosity
  | Added
  | Modified
  | Redescribed
  | Removed
  | Checked
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

updateWrapper
  :: (MonadIO m, MonadError AppError m)
  => Maybe String
  -> Maybe String
  -> Entry
  -> m Entry
updateWrapper Nothing Nothing e =
  pure e
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
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertextWrapper Change e =
  promptText "Enter text to encrypt: " >>= \t -> updateCiphertext (Plaintext . T.pack $ t) e
updateCiphertextWrapper Keep e =
  pure e

modifyOnlySingletons
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => [Entry]
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
modifyOnlySingletons [e] maction miden mmeta = do
  ctx <- ask
  ue1 <- updateWrapper miden mmeta e
  ue2 <- updateCiphertextWrapper maction ue1
  _   <- DB.put (appContextConnection ctx) ue2
  _   <- DB.delete (appContextConnection ctx) e
  return Modified
modifyOnlySingletons _ _ _ _ =
  throwError (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndModify
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Query
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
findAndModify q maction miden mmeta = do
  ctx <- ask
  rs  <- DB.query (appContextConnection ctx) q
  modifyOnlySingletons rs maction miden mmeta

modify
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
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
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => [Entry]
  -> String
  -> m Response
redescribeOnlySingletons [e] s = do
  ctx <- ask
  ue  <- updateDescription (Description . T.pack $ s) e
  _   <- DB.put (appContextConnection ctx) ue
  _   <- DB.delete (appContextConnection ctx) e
  return Redescribed
redescribeOnlySingletons _ _ =
  throwError (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndRedescribe
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Query
  -> String
  -> m Response
findAndRedescribe q s = do
  ctx <- ask
  rs  <- DB.query (appContextConnection ctx) q
  redescribeOnlySingletons rs s

redescribe
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Target
  -> String
  -> m Response
redescribe (TargetId tid) s =
  findAndRedescribe (query (Just tid) Nothing Nothing Nothing) s
redescribe (TargetDescription tdesc) s =
  findAndRedescribe (query Nothing (Just tdesc) Nothing Nothing) s

removeOnlySingletons
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => [Entry]
  -> m Response
removeOnlySingletons [e] = do
  ctx <- ask
  _   <- DB.delete (appContextConnection ctx) e
  return Removed
removeOnlySingletons _ =
  throwError (AmbiguousInput "There are multiple entries matching your input criteria.")

findAndRemove
  :: (MonadIO m, MonadReader AppContext m,  MonadError AppError m)
  => Query
  -> m Response
findAndRemove q = do
  ctx <- ask
  rs  <- DB.query (appContextConnection ctx) q
  removeOnlySingletons rs

remove
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Target
  -> m Response
remove (TargetId rid) =
  findAndRemove (query (Just rid) Nothing Nothing Nothing)
remove (TargetDescription rdesc) =
  findAndRemove (query Nothing (Just rdesc) Nothing Nothing)

check
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => m Response
check = do
  ctx <- ask
  r   <- DB.checkEntries (appContextConnection ctx) (appContextKeyId ctx)
  if r
    then return Checked
    else throwError (CheckError "failed")

eval
  :: (MonadIO m, MonadError AppError m, MonadReader AppContext m)
  => Command
  -> m Response
eval Add{addDescription, addIdentity, addMeta} = do
  ctx <- ask
  t   <- promptText "Enter text to encrypt: "
  e   <- createEntryWrapper addDescription addIdentity addMeta t
  _   <- DB.put (appContextConnection ctx) e
  return Added
eval Lookup{lookupDescription, lookupVerbosity} = do
  ctx <- ask
  q   <- pure $ query Nothing (Just lookupDescription) Nothing Nothing
  res <- DB.query (appContextConnection ctx) q
  case res of
    []  -> MultipleEntries <$> pure []           <*> pure lookupVerbosity
    [e] -> SingleEntry     <$> decryptEntry e    <*> pure lookupVerbosity
    es  -> MultipleEntries <$> decryptEntries es <*> pure lookupVerbosity
eval Import{importFile} = do
  ctx <- ask
  es  <- importCSV importFile
  _   <- mapM_ (DB.put (appContextConnection ctx)) es
  return Added
eval (Modify t c i m) = modify t c i m
eval (Redescribe t s) = redescribe t s
eval (Remove t)       = remove t
eval Check            = check
