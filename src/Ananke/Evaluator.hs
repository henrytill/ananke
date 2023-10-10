{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ananke.Evaluator
  ( ModifyAction(..)
  , Verbosity(..)
  , Target(..)
  , Command(..)
  , Response(..)
  , setup
  , eval
  ) where

import           Prelude                  hiding (lookup)

#ifdef BACKEND_JSON
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.List                as List
#endif

import           Data.Char                (toLower)
import           Data.Time.Clock          (UTCTime)

import           Ananke.Data
import           Ananke.Interfaces


currentSchemaVersion :: SchemaVersion
currentSchemaVersion = MkSchemaVersion 2

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
#ifdef BACKEND_JSON
  | Import { importFile :: FilePath }
  | Export { exportFile :: FilePath }
#endif
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
queryFromTarget (TargetId targetId)                   = MkQuery (Just targetId) Nothing                  Nothing Nothing
queryFromTarget (TargetDescription targetDescription) = MkQuery Nothing         (Just targetDescription) Nothing Nothing

getSchemaVersionFromFile :: MonadInteraction m => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = MkSchemaVersion . read <$> readFileAsString path

createSchemaFile :: MonadInteraction m => FilePath -> SchemaVersion -> m SchemaVersion
createSchemaFile path version = writeFileFromString path (show version) >> return version

getSchemaVersion :: (MonadInteraction m, MonadStore m) => FilePath -> m SchemaVersion
getSchemaVersion path = do
  fileExists <- doesFileExist path
  if fileExists
    then getSchemaVersionFromFile path
    else createSchemaFile path currentSchemaVersion

reencryptAll :: forall m. (MonadEncrypt m, MonadStore m) => KeyId -> m ()
reencryptAll keyId = do
  entries        <- selectAll
  updatedEntries <- mapM (reencrypt keyId) entries
  mapM_ put    updatedEntries
  mapM_ delete entries
  where
    reencrypt :: KeyId -> Entry -> m Entry
    reencrypt entryKeyId entry = do
      plaintext       <- decrypt $ entryCiphertext entry
      entryCiphertext <- encrypt entryKeyId plaintext
      return $ updateEntry entry{entryKeyId, entryCiphertext}

initDatabase
  :: (MonadEncrypt m, MonadInteraction m, MonadStore m)
  => FilePath
  -> SchemaVersion
  -> KeyId
  -> m ()
initDatabase path schemaVersion keyId =
  if schemaVersion == currentSchemaVersion
  then createTable
  else do message ("Migrating database from schema version "
                   ++ show schemaVersion
                   ++ " to version "
                   ++ show currentSchemaVersion
                   ++ "...")
          migrate schemaVersion keyId
          reencryptAll keyId
          _ <- createSchemaFile path currentSchemaVersion
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
  :: (MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Description
  -> Maybe Identity
  -> Maybe Metadata
  -> m Response
add description maybeIdentity maybeMeta = do
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
    []      -> return $ MultipleEntries [] verbosity
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
  let aesonCfg = AesonPretty.defConfig{AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}
      output   = AesonPretty.encodePretty' aesonCfg . List.sort $ entries
  writeFileFromLazyByteString jsonFile output
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
  entryCiphertext <- encrypt keyId $ mkPlaintext input
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
modify target modifyAction maybeIdentity maybeMeta = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> now >>= updateCiphertext modifyAction . update maybeIdentity maybeMeta entry >>= put >> delete entry >> return Modified
    []      -> ambiguousInputError "There are no entries matching your input criteria."
    _       -> ambiguousInputError "There are multiple entries matching your input criteria."

redescribe
  :: (MonadAppError m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => Target
  -> Description
  -> m Response
redescribe target entryDescription = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> do entryTimestamp <- now
                  put $ updateEntry entry{entryTimestamp, entryDescription}
                  delete entry
                  return Redescribed
    _       -> ambiguousInputError "There are multiple entries matching your input criteria."

remove :: (MonadAppError m, MonadStore m) => Target -> m Response
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
eval Add{addDescription, addIdentity, addMeta}                          = checkKey $ add addDescription addIdentity addMeta
eval Lookup{lookupDescription, lookupIdentity, lookupVerbosity}         = lookup lookupDescription lookupIdentity lookupVerbosity
eval Modify{modifyTarget, modifyCiphertext, modifyIdentity, modifyMeta} = checkKey $ modify modifyTarget modifyCiphertext modifyIdentity modifyMeta
eval Redescribe{redescribeTarget, redescribeDescription}                = checkKey $ redescribe redescribeTarget redescribeDescription
eval Remove{removeTarget}                                               = remove removeTarget
eval CheckForMultipleKeys                                               = check
#ifdef BACKEND_JSON
eval Import{importFile}                                                 = importJSON importFile
eval Export{exportFile}                                                 = exportJSON exportFile
#endif