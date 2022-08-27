{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , eval
  , setup
  ) where

#ifdef BACKEND_JSON
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.List                as List
#endif

import qualified Data.Char                as Char
import qualified Data.Text                as T

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
  = Add { addDescription :: String
        , addIdentity    :: Maybe String
        , addMeta        :: Maybe String
        }
  | Lookup { lookupDescription :: String
           , lookupIdentity    :: Maybe String
           , lookupVerbosity   :: Verbosity
           }
  | ExportJSON { exportFile :: FilePath }
  | Modify { modifyTarget     :: Target
           , modifyCiphertext :: ModifyAction
           , modifyIdentity   :: Maybe String
           , modifyMeta       :: Maybe String
           }
  | Redescribe { redescribeTarget      :: Target
               , redescribeDescription :: String
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
getSchemaVersionFromFile path = MkSchemaVersion . read <$> readFileAsString path

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
    else do message ("Migrating database from schema version "
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
  let schemaFile = configSchemaFile cfg
      keyId      = configKeyId cfg
  schemaVersion <- getSchemaVersion schemaFile
  initDatabase schemaFile schemaVersion keyId

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
  let keyId         = configKeyId cfg
      allowMultKeys = configAllowMultipleKeys cfg
  total <- getCount
  if total == 0
    then k
    else do let q     = "New keyid found: do you want to re-encrypt all entries?"
                reenc = reencryptAll keyId
                err   = defaultError "You have set allow_multiple_keys to false"
            count <- getCountOfKeyId keyId
            case (count, allowMultKeys) of
              (0, False)              -> binaryChoice q (reenc >> k) err
              (x, _    ) | x == total -> k
              (_, True )              -> binaryChoice q (reenc >> k) k
              (_, False)              -> defaultError "All entries do not have the same keyid"

#ifdef BACKEND_JSON
exportJSON :: (MonadInteraction m) => FilePath -> [Entry] -> m ()
exportJSON jsonFile entries = writeFileFromLazyByteString jsonFile json
  where
    cfg  = Aeson.defConfig{Aeson.confCompare = Aeson.keyOrder entryKeyOrder}
    json = Aeson.encodePretty' cfg (List.sort entries)
#else
exportJSON :: (MonadAppError m, MonadInteraction m) => FilePath -> [Entry] -> m ()
exportJSON _  _ = defaultError "No JSON support.  Please rebuild with backend-json flag enabled."
#endif

create
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => String
  -> Maybe String
  -> Maybe String
  -> m Entry
create d i m = do
  t         <- prompt "Enter text to encrypt: "
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (configKeyId cfg)
              timestamp
              (MkDescription . T.pack  $  d)
              (MkIdentity    . T.pack <$> i)
              (MkPlaintext   . T.pack  $  t)
              (MkMetadata    . T.pack <$> m)

update
  :: MonadInteraction m
  => Maybe String
  -> Maybe String
  -> Entry
  -> m Entry
update Nothing Nothing e =
  return e
update (Just "") Nothing e =
  do ts <- now
     return . updateIdentity ts Nothing $ e
update miden Nothing e =
  do ts <- now
     return . updateIdentity ts (MkIdentity . T.pack <$> miden) $ e
update Nothing (Just "") e =
  do ts <- now
     return . updateMetadata ts Nothing $ e
update Nothing mmeta e =
  do ts <- now
     return . updateMetadata ts (MkMetadata . T.pack <$> mmeta) $ e
update (Just "") (Just "") e =
  do ts <- now
     return . updateMetadata ts Nothing . updateIdentity ts Nothing $ e
update miden mmeta e =
  do ts <- now
     return . updateMetadata ts (MkMetadata . T.pack <$> mmeta) . updateIdentity ts (MkIdentity . T.pack <$> miden) $ e

updateCiphertext
  :: (MonadEncrypt m, MonadInteraction m, MonadConfigReader m)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertext Change ent = do
  t         <- prompt "Enter text to encrypt: "
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (configKeyId cfg)
              timestamp
              (entryDescription ent)
              (entryIdentity ent)
              (MkPlaintext . T.pack $ t)
              (entryMeta ent)
updateCiphertext Keep ent =
  return ent

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
  ue1 <- update miden mmeta e
  ue2 <- updateCiphertext maction ue1
  _   <- put ue2
  _   <- delete e
  return Modified
modifyOnlySingletons [] _ _ _ =
  ambiguousInputError "There are no entries matching your input criteria."
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
findAndModify q maction miden mmeta =
  checkKey (query q >>= \rs -> modifyOnlySingletons rs maction miden mmeta)

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
      let ue = updateDescription ts (MkDescription . T.pack $ s) e
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
  let keyId = configKeyId cfg
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
eval Add{addDescription, addIdentity, addMeta} =
  checkKey $ create addDescription addIdentity addMeta >>= put >> return Added
eval Lookup{lookupDescription, lookupIdentity, lookupVerbosity} = do
  let q = Data.query Nothing (Just lookupDescription) lookupIdentity Nothing
  res <- query q
  case res of
    []  -> return (MultipleEntries [] lookupVerbosity)
    [e] -> SingleEntry     <$> entryToDisplayEntry decrypt e         <*> pure lookupVerbosity
    es  -> MultipleEntries <$> mapM (entryToDisplayEntry decrypt) es <*> pure lookupVerbosity
eval ExportJSON{exportFile} = do
  es  <- selectAll
  _   <- exportJSON exportFile es
  return Exported
eval (Modify t c i m)     = modify t c i m
eval (Redescribe t s)     = redescribe t s
eval (Remove t)           = remove t
eval CheckForMultipleKeys = check
