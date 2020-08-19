{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , importCSV
  , exportCSV
  , eval
  , setup
  ) where

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Char                as Char
import qualified Data.Csv                 as CSV
import qualified Data.List                as List
import qualified Data.Text                as T
import qualified Data.Vector              as Vector
import           Lens.Family2

import           Hecate.Data              hiding (query)
import qualified Hecate.Data              as Data
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
           , _lookupIdentity    :: Maybe String
           , _lookupVerbosity   :: Verbosity
           }
  | Import { _importFile :: FilePath }
  | Export { _exportFile :: FilePath }
  | ExportJSON { _exportFile :: FilePath }
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

getSchemaVersionFromFile :: MonadInteraction m => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = SchemaVersion . read <$> readFileAsString path

createSchemaFile :: MonadInteraction m => FilePath -> SchemaVersion -> m ()
createSchemaFile path version = writeFileFromString path (show version)

getSchemaVersion :: (MonadInteraction m, MonadStore m) => FilePath -> m SchemaVersion
getSchemaVersion path = do
  version <- currentSchemaVersion
  exists  <- doesFileExist path
  if exists
    then getSchemaVersionFromFile path
    else createSchemaFile path version >> return version

initDatabase
  :: (MonadInteraction m, MonadEncrypt m, MonadStore m)
  => FilePath
  -> SchemaVersion
  -> KeyId
  -> m ()
initDatabase path schemaVersion keyId = do
  current <- currentSchemaVersion
  if schemaVersion == current
  then createTable
  else do
    message ("Migrating database from schema version "
             ++ show schemaVersion
             ++ " to version "
             ++ show current
             ++ "...")
    migrate schemaVersion keyId
    reencryptAll keyId
    createSchemaFile path current

setup
  :: ( MonadInteraction m
     , MonadEncrypt m
     , MonadStore m
     , MonadConfigReader m
     )
  => m ()
setup = do
  cfg <- askConfig
  let schemaFile = cfg ^. configSchemaFile
      keyId      = cfg ^. configKeyId
  schemaVersion <- getSchemaVersion schemaFile
  initDatabase schemaFile schemaVersion keyId

ensureFile :: (MonadAppError m, MonadInteraction m) => FilePath -> m FilePath
ensureFile file = do
  exists <- doesFileExist file
  if exists
    then return file
    else fileSystemError "File does not exist"

reencryptAll
  :: (MonadEncrypt m, MonadStore m)
  => KeyId
  -> m ()
reencryptAll keyId = do
  es  <- selectAll
  ues <- mapM (updateKeyId decrypt encrypt keyId) es
  mapM_ put  ues
  mapM_ delete es

binaryChoice
  :: (MonadAppError m, MonadInteraction m)
  => String
  -> m a
  -> m a
  -> m a
binaryChoice s yes no = do
  ans <- prompt (s ++ " [N/y] ")
  case map Char.toLower ans of
    ""  -> no
    "n" -> no
    "y" -> yes
    _   -> defaultError "Please answer y or n"

checkKey
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => m Response
  -> m Response
checkKey k = do
  cfg <- askConfig
  let keyId         = cfg ^. configKeyId
      allowMultKeys = cfg ^. configAllowMultipleKeys
  total <- getCount
  if total == 0
    then k
    else do
    let q     = "New keyid found: do you want to re-encrypt all entries?"
        reenc = reencryptAll keyId
        err   = defaultError "You have set allow_multiple_keys to false"
    count <- getCountOfKeyId keyId
    case (count, allowMultKeys) of
      (0, False)              -> binaryChoice q (reenc >> k) err
      (x, _    ) | x == total -> k
      (_, True )              -> binaryChoice q (reenc >> k) k
      (_, False)              -> defaultError "All entries do not have the same keyid"

csvEntryToEntry
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => CSVEntry
  -> m Entry
csvEntryToEntry entry = do
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (cfg ^. configKeyId)
              timestamp
              (_csvDescription entry)
              (_csvIdentity entry)
              (_csvPlaintext entry)
              (_csvMeta entry)

importCSV
  :: (MonadAppError m, MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => FilePath
  -> m [Entry]
importCSV csvFile = do
  file <- ensureFile csvFile
  bs   <- readFileAsLazyByteString file
  ies  <- either csvDecodingError (pure . Vector.toList) (CSV.decode CSV.NoHeader bs)
  mapM csvEntryToEntry ies

exportCSV
  :: (MonadInteraction m, MonadEncrypt m)
  => FilePath
  -> [Entry]
  -> m ()
exportCSV csvFile entries = do
  csvEntries <- mapM (entryToCSVEntry decrypt) entries
  let csv = CSV.encode csvEntries
  writeFileFromLazyByteString csvFile csv

exportJSON :: (MonadInteraction m) => FilePath -> [Entry] -> m ()
exportJSON jsonFile entries = writeFileFromLazyByteString jsonFile json
  where
    cfg  = Aeson.defConfig{Aeson.confCompare = Aeson.keyOrder entryKeyOrder}
    json = Aeson.encodePretty' cfg (List.sort entries)

createEntryWrapper
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => String
  -> Maybe String
  -> Maybe String
  -> String
  -> m Entry
createEntryWrapper d i m t = do
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (cfg ^. configKeyId)
              timestamp
              (Description . T.pack  $  d)
              (Identity    . T.pack <$> i)
              (Plaintext   . T.pack  $  t)
              (Metadata    . T.pack <$> m)

updateWrapper
  :: MonadInteraction m
  => Maybe String
  -> Maybe String
  -> Entry
  -> m Entry
updateWrapper Nothing Nothing e =
  return e
updateWrapper (Just "") Nothing e =
  now >>= \ ts -> updateIdentity ts Nothing e
updateWrapper miden Nothing e =
  now >>= \ ts -> updateIdentity ts (Identity . T.pack <$> miden) e
updateWrapper Nothing (Just "") e =
  now >>= \ ts -> updateMetadata ts Nothing e
updateWrapper Nothing mmeta e =
  now >>= \ ts -> updateMetadata ts (Metadata . T.pack <$> mmeta) e
updateWrapper (Just "") (Just "") e =
  now                         >>= \ ts ->
  updateIdentity ts Nothing e >>=
  updateMetadata ts Nothing
updateWrapper miden mmeta e =
  now                                               >>= \ ts ->
  updateIdentity ts (Identity . T.pack <$> miden) e >>=
  updateMetadata ts (Metadata . T.pack <$> mmeta)

updateCiphertext
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => Plaintext
  -> Entry
  -> m Entry
updateCiphertext plaintext entry = do
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (cfg ^. configKeyId)
              timestamp
              (_entryDescription entry)
              (_entryIdentity entry)
              plaintext
              (_entryMeta entry)

updateCiphertextWrapper
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertextWrapper Change e =
  prompt "Enter text to encrypt: " >>= \ t -> updateCiphertext (Plaintext . T.pack $ t) e
updateCiphertextWrapper Keep e =
  return e

modifyOnlySingletons
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
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
  ambiguousInputError "There are multiple entries matching your input criteria."

findAndModify
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => Query
  -> ModifyAction
  -> Maybe String
  -> Maybe String
  -> m Response
findAndModify q maction miden mmeta
  = checkKey (query q >>= \ rs -> modifyOnlySingletons rs maction miden mmeta)

modify
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
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
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => [Entry]
  -> String
  -> m Response
redescribeOnlySingletons [e] s
  = checkKey k
  where
    k = do
      ts  <- now
      ue  <- updateDescription ts (Description . T.pack $ s) e
      _   <- put ue
      _   <- delete e
      return Redescribed
redescribeOnlySingletons _ _ =
  ambiguousInputError "There are multiple entries matching your input criteria."

findAndRedescribe
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => Query
  -> String
  -> m Response
findAndRedescribe q s = do
  rs  <- query q
  redescribeOnlySingletons rs s

redescribe
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => Target
  -> String
  -> m Response
redescribe (TargetId tid) s =
  findAndRedescribe (Data.query (Just tid) Nothing Nothing Nothing) s
redescribe (TargetDescription tdesc) s =
  findAndRedescribe (Data.query Nothing (Just tdesc) Nothing Nothing) s

removeOnlySingletons
  :: (MonadAppError m, MonadStore m, MonadConfigReader m)
  => [Entry]
  -> m Response
removeOnlySingletons [e] =
  delete e >> return Removed
removeOnlySingletons _ =
  ambiguousInputError "There are multiple entries matching your input criteria."

findAndRemove
  :: (MonadAppError m, MonadStore m, MonadConfigReader m)
  => Query
  -> m Response
findAndRemove q =
  query q >>= removeOnlySingletons

remove
  :: (MonadAppError m, MonadStore m, MonadConfigReader m)
  => Target
  -> m Response
remove (TargetId rid) =
  findAndRemove (Data.query (Just rid) Nothing Nothing Nothing)
remove (TargetDescription rdesc) =
  findAndRemove (Data.query Nothing (Just rdesc) Nothing Nothing)

check
  :: (MonadAppError m, MonadStore m, MonadConfigReader m)
  => m Response
check = do
  cfg <- askConfig
  t   <- getCount
  let keyId = cfg ^. configKeyId
  r <- getCountOfKeyId keyId
  if t == r
    then return CheckedForMultipleKeys
    else defaultError "All entries do not have the same keyid"

eval
  :: ( MonadAppError m
     , MonadEncrypt m
     , MonadInteraction m
     , MonadStore m
     , MonadConfigReader m
     )
  => Command
  -> m Response
eval Add{_addDescription, _addIdentity, _addMeta} =
  checkKey k
  where
    k = prompt "Enter text to encrypt: "                         >>=
        createEntryWrapper _addDescription _addIdentity _addMeta >>=
        put                                                      >>
        return Added
eval Lookup{_lookupDescription, _lookupIdentity, _lookupVerbosity} = do
  let q = Data.query Nothing (Just _lookupDescription) _lookupIdentity Nothing
  res <- query q
  case res of
    []  -> pure (MultipleEntries [] _lookupVerbosity)
    [e] -> SingleEntry     <$> entryToDisplayEntry decrypt e         <*> pure _lookupVerbosity
    es  -> MultipleEntries <$> mapM (entryToDisplayEntry decrypt) es <*> pure _lookupVerbosity
eval Import{_importFile} =
  checkKey (importCSV _importFile >>= mapM_ put >> return Added)
eval Export{_exportFile}  = do
  es  <- selectAll
  _   <- exportCSV _exportFile es
  return Exported
eval ExportJSON{_exportFile} = do
  es  <- selectAll
  _   <- exportJSON _exportFile es
  return Exported
eval (Modify t c i m)     = modify t c i m
eval (Redescribe t s)     = redescribe t s
eval (Remove t)           = remove t
eval CheckForMultipleKeys = check
