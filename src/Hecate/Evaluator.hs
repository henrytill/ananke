{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hecate.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , setup
  , eval
  ) where

#ifdef BACKEND_JSON
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.List                as List
#endif

import           Data.Char                (toLower)
import           Data.Time.Clock          (UTCTime)
import           Prelude                  hiding (lookup)

import           Hecate.Data
import           Hecate.Interfaces


data ModifyAction = Keep | Change
  deriving (Show, Eq)

data Verbosity = Normal | Verbose
  deriving (Show, Eq)

data Target
  = TargetId Id
  | TargetDescription Description
  deriving (Show, Eq)

-- | 'Command' represents CLI commands
data Command
  = Add { addDescription :: Description
        , addIdentity    :: Maybe Identity
        , addMeta        :: Maybe Metadata
        }
  | Lookup { lookupDescription :: Description
           , lookupIdentity    :: Maybe Identity
           , lookupVerbosity   :: Verbosity
           }
#ifdef BACKEND_JSON
  | ImportJSON { importFile :: FilePath }
  | ExportJSON { exportFile :: FilePath }
#endif
  | Modify { modifyTarget     :: Target
           , modifyCiphertext :: ModifyAction
           , modifyIdentity   :: Maybe Identity
           , modifyMeta       :: Maybe Metadata
           }
  | Redescribe { redescribeTarget      :: Target
               , redescribeDescription :: Description
               }
  | Remove { removeTarget :: Target }
  | CheckForMultipleKeys
  deriving Show

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry Verbosity
  | MultipleEntries [DisplayEntry] Verbosity
  | Added
  | Imported
  | Exported
  | Modified
  | Redescribed
  | Removed
  | CheckedForMultipleKeys
  deriving (Show, Eq)

queryFromTarget :: Target -> Query
queryFromTarget (TargetId targetId)                   = MkQuery (Just targetId) Nothing Nothing Nothing
queryFromTarget (TargetDescription targetDescription) = MkQuery Nothing (Just targetDescription) Nothing Nothing

getSchemaVersionFromFile :: MonadInteraction m => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = MkSchemaVersion . read <$> readFileAsString path

createSchemaFile :: MonadInteraction m => FilePath -> SchemaVersion -> m SchemaVersion
createSchemaFile path version = writeFileFromString path (show version) >> return version

getSchemaVersion :: (MonadInteraction m, MonadStore m) => FilePath -> m SchemaVersion
getSchemaVersion path = do
  fileExists <- doesFileExist path
  if fileExists
    then getSchemaVersionFromFile path
    else currentSchemaVersion >>= createSchemaFile path

reencryptAll :: forall m. (MonadEncrypt m, MonadStore m) => KeyId -> m ()
reencryptAll keyId = do
  entries        <- selectAll
  updatedEntries <- mapM reencrypt entries
  mapM_ put    updatedEntries
  mapM_ delete entries
  where
    reencrypt :: Entry -> m Entry
    reencrypt entry = do
      plaintext  <- decrypt $ entryCiphertext entry
      ciphertext <- encrypt keyId plaintext
      return $ mkEntry keyId
                       (entryTimestamp entry)
                       (entryDescription entry)
                       (entryIdentity entry)
                       ciphertext
                       (entryMeta entry)

initDatabase
  :: (MonadEncrypt m, MonadInteraction m, MonadStore m)
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
            _ <- createSchemaFile path current
            return ()

setup :: (MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m) => m ()
setup = do
  cfg <- askConfig
  let schemaFile = configSchemaFile cfg
      keyId      = configKeyId cfg
  schemaVersion <- getSchemaVersion schemaFile
  initDatabase schemaFile schemaVersion keyId

checkKey
  :: forall m. (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => m Response
  -> m Response
checkKey k = do
  totalCount <- getCount
  if totalCount == 0
    then k
    else do cfg <- askConfig
            let keyId         = configKeyId cfg
                allowMultKeys = configAllowMultipleKeys cfg
                question      = "New keyid found: do you want to re-encrypt all entries?"
                err           = defaultError "You have set allow_multiple_keys to false"
            keyCount <- getCountOfKeyId keyId
            case (keyCount, allowMultKeys) of
              (0, False)                   -> yesNo question (reencryptAll keyId >> k) err
              (x, _    ) | x == totalCount -> k
              (_, True )                   -> yesNo question (reencryptAll keyId >> k) k
              (_, False)                   -> defaultError "All entries do not have the same keyid"
  where
    yesNo :: String -> m a -> m a -> m a
    yesNo promptString kYes kNo = do
      input <- prompt (promptString ++ " [N/y] ")
      case map toLower input of
        ""  -> kNo
        "n" -> kNo
        "y" -> kYes
        _   -> defaultError "Please answer y or n"

add
  :: (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Description
  -> Maybe Identity
  -> Maybe Metadata
  -> m Response
add description maybeIdentity maybeMeta = checkKey $ do
  input      <- prompt "Enter text to encrypt: "
  timestamp  <- now
  keyId      <- configKeyId <$> askConfig
  ciphertext <- encrypt keyId $ mkPlaintext input
  put $ mkEntry keyId timestamp description maybeIdentity ciphertext maybeMeta
  return Added

lookup :: forall m. (MonadEncrypt m, MonadStore m) => Description -> Maybe Identity -> Verbosity -> m Response
lookup description maybeIdentity verbosity = do
  entries <- runQuery $ MkQuery Nothing (Just description) maybeIdentity Nothing
  case entries of
    []      -> return (MultipleEntries [] verbosity)
    [entry] -> SingleEntry     <$> f entry        <*> pure verbosity
    _       -> MultipleEntries <$> mapM f entries <*> pure verbosity
  where
    f :: Entry -> m DisplayEntry
    f MkEntry{entryId, entryTimestamp, entryDescription, entryIdentity, entryCiphertext, entryMeta} =
      do plaintext <- decrypt entryCiphertext
         return $ MkDisplayEntry entryId entryTimestamp entryDescription entryIdentity plaintext entryMeta

#ifdef BACKEND_JSON
importJSON :: (MonadAppError m, MonadInteraction m, MonadStore m) => FilePath -> m Response
importJSON jsonFile = do
  input   <- readFileAsLazyByteString jsonFile
  entries <- maybe (defaultError ("unable to decode " ++ jsonFile)) return (Aeson.decode input :: Maybe [Entry])
  mapM_ put entries
  return Imported

exportJSON :: (MonadInteraction m, MonadStore m) => FilePath -> m Response
exportJSON jsonFile = do
  entries <- selectAll
  let cfg  = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}
      json = AesonPretty.encodePretty' cfg (List.sort entries)
  writeFileFromLazyByteString jsonFile json
  return Exported
#endif

update :: Maybe Identity -> Maybe Metadata -> Entry -> UTCTime -> Entry
update Nothing                Nothing                entry _              = entry
update (Just (MkIdentity "")) Nothing                entry entryTimestamp = updateEntry entry{entryTimestamp, entryIdentity = Nothing}
update entryIdentity          Nothing                entry entryTimestamp = updateEntry entry{entryTimestamp, entryIdentity}
update Nothing                (Just (MkMetadata "")) entry entryTimestamp = updateEntry entry{entryTimestamp, entryMeta = Nothing}
update Nothing                entryMeta              entry entryTimestamp = updateEntry entry{entryTimestamp, entryMeta}
update (Just (MkIdentity "")) (Just (MkMetadata "")) entry entryTimestamp = updateEntry entry{entryTimestamp, entryIdentity = Nothing, entryMeta = Nothing}
update entryIdentity          entryMeta              entry entryTimestamp = updateEntry entry{entryTimestamp, entryIdentity, entryMeta}

updateCiphertext
  :: (MonadConfigReader m, MonadEncrypt m, MonadInteraction m)
  => ModifyAction
  -> Entry
  -> m Entry
updateCiphertext Change entry = do
  input           <- prompt "Enter text to encrypt: "
  entryTimestamp  <- now
  keyId           <- configKeyId <$> askConfig
  entryCiphertext <- encrypt keyId (mkPlaintext input)
  return $ updateEntry entry{entryTimestamp, entryCiphertext}
updateCiphertext Keep entry =
  return entry

modify
  :: (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Target
  -> ModifyAction
  -> Maybe Identity
  -> Maybe Metadata
  -> m Response
modify target modifyAction maybeIdentity maybeMeta = checkKey $ do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> now >>= updateCiphertext modifyAction . update maybeIdentity maybeMeta entry >>= put >> delete entry >> return Modified
    []      -> ambiguousInputError "There are no entries matching your input criteria."
    _       -> ambiguousInputError "There are multiple entries matching your input criteria."

redescribe
  :: (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Target
  -> Description
  -> m Response
redescribe target entryDescription = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> checkKey $ do entryTimestamp <- now
                             put $ updateEntry entry{entryTimestamp, entryDescription}
                             delete entry
                             return Redescribed
    _       -> ambiguousInputError "There are multiple entries matching your input criteria."

remove :: (MonadAppError m, MonadConfigReader m, MonadStore m) => Target -> m Response
remove target = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> delete entry >> return Removed
    _       -> ambiguousInputError "There are multiple entries matching your input criteria."

check :: (MonadAppError m, MonadConfigReader m, MonadStore m) => m Response
check = do
  keyId      <- configKeyId <$> askConfig
  totalCount <- getCount
  keyCount   <- getCountOfKeyId keyId
  if totalCount == keyCount
    then return CheckedForMultipleKeys
    else defaultError "All entries do not have the same keyid"

eval
  :: (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Command
  -> m Response
eval Add{addDescription, addIdentity, addMeta}                          = add addDescription addIdentity addMeta
eval Lookup{lookupDescription, lookupIdentity, lookupVerbosity}         = lookup lookupDescription lookupIdentity lookupVerbosity
#ifdef BACKEND_JSON
eval ImportJSON{importFile}                                             = importJSON importFile
eval ExportJSON{exportFile}                                             = exportJSON exportFile
#endif
eval Modify{modifyTarget, modifyCiphertext, modifyIdentity, modifyMeta} = modify modifyTarget modifyCiphertext modifyIdentity modifyMeta
eval Redescribe{redescribeTarget, redescribeDescription}                = redescribe redescribeTarget redescribeDescription
eval Remove{removeTarget}                                               = remove removeTarget
eval CheckForMultipleKeys                                               = check
