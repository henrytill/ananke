{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ananke.Evaluator
  ( Target (..),
    Command (..),
    Response (..),
    eval,
    showVersion,
  )
where

import Ananke.Class
import Ananke.Data
import Control.Applicative ((<|>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Char (toLower)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Time.Clock (UTCTime)
import Data.Version qualified as Version
import Paths_ananke qualified
import Prelude hiding (lookup)

type IsVerbose = Bool

data Target
  = TargetId Id
  | TargetDescription Description
  deriving (Show, Eq)

-- | 'Command' represents CLI commands
data Command
  = Add
      { addDescription :: Description,
        addIdentity :: Maybe Identity,
        addMeta :: Maybe Metadata
      }
  | Lookup
      { lookupDescription :: Description,
        lookupIdentity :: Maybe Identity,
        lookupVerbose :: IsVerbose
      }
  | Modify
      { modifyTarget :: Target,
        modifyPlaintext :: Bool,
        modifyIdentity :: Maybe Identity,
        modifyMeta :: Maybe Metadata
      }
  | Remove {removeTarget :: Target}
  | CheckForMultipleKeys
  | Import {importFile :: FilePath}
  | Export {exportFile :: FilePath}
  deriving (Show)

-- | 'Response' represents the response to a 'Command'
data Response
  = SingleEntry DisplayEntry IsVerbose
  | MultipleEntries [DisplayEntry] IsVerbose
  | Added
  | Imported
  | Exported
  | Modified
  | Redescribed
  | Removed
  | CheckedForMultipleKeys
  deriving (Show, Eq)

queryFromTarget :: Target -> Query
queryFromTarget (TargetId ti) = emptyQuery {queryId = Just ti}
queryFromTarget (TargetDescription td) = emptyQuery {queryDescription = Just td}

reencryptAll :: forall m. (MonadEncrypt m, MonadStore m) => KeyId -> m ()
reencryptAll keyId = do
  entries <- selectAll
  updatedEntries <- mapM (reencrypt keyId) entries
  mapM_ put updatedEntries
  mapM_ delete entries
  where
    reencrypt :: KeyId -> Entry -> m Entry
    reencrypt entryKeyId entry = do
      plaintext <- decrypt $ entryCiphertext entry
      entryCiphertext <- encrypt entryKeyId plaintext
      return $ updateEntry entry {entryKeyId, entryCiphertext}

checkKey ::
  forall m.
  (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m) =>
  m Response ->
  m Response
checkKey k = do
  count <- getCount
  if count == 0
    then k
    else askUpdate count
  where
    askUpdate :: Int -> m Response
    askUpdate totalCount = do
      cfg <- askConfig
      let keyId = configKeyId cfg
          allowMultKeys = configMultKeys cfg
          question = "New keyid found: do you want to re-encrypt all entries?"
          err = throwDefault "You have set allow_multiple_keys to false"
      keyCount <- getCountOf keyId
      case (keyCount, allowMultKeys) of
        (0, False) -> promptYesNo question (reencryptAll keyId >> k) err
        (x, _) | x == totalCount -> k
        (_, True) -> promptYesNo question (reencryptAll keyId >> k) k
        (_, False) -> throwDefault "All entries do not have the same keyid"
    promptYesNo :: String -> m a -> m a -> m a
    promptYesNo q ky kn = do
      input <- prompt (q ++ " [N/y] ")
      case toLower <$> input of
        "" -> kn
        "n" -> kn
        "y" -> ky
        _ -> throwDefault "Please answer y or n"

add ::
  (MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m, MonadTime m) =>
  Description ->
  Maybe Identity ->
  Maybe Metadata ->
  m Response
add description maybeIdentity maybeMeta = do
  input <- prompt "Enter text to encrypt: "
  timestamp <- now
  keyId <- configKeyId <$> askConfig
  ciphertext <- encrypt keyId $ mkPlaintext input
  put $ mkEntry timestamp keyId description maybeIdentity ciphertext maybeMeta
  return Added

lookup :: forall m. (MonadEncrypt m, MonadStore m) => Description -> Maybe Identity -> IsVerbose -> m Response
lookup description maybeIdentity isVerbose = do
  entries <- runQuery $ MkQuery Nothing (Just description) maybeIdentity Nothing
  case entries of
    [] -> return $ MultipleEntries [] isVerbose
    [entry] -> SingleEntry <$> f entry <*> pure isVerbose
    _ -> MultipleEntries <$> mapM f entries <*> pure isVerbose
  where
    f :: Entry -> m DisplayEntry
    f entry = do
      plaintext <- decrypt (entryCiphertext entry)
      return $
        MkDisplayEntry
          (entryTimestamp entry)
          (entryId entry)
          (entryKeyId entry)
          (entryDescription entry)
          (entryIdentity entry)
          plaintext
          (entryMeta entry)

update :: Maybe Description -> Maybe Identity -> Maybe Metadata -> Entry -> UTCTime -> Entry
update Nothing Nothing Nothing entry _ = entry
update maybeDescription maybeIdentity maybeMetadata entry entryTimestamp =
  updateEntry $
    entry
      { entryDescription = Maybe.fromMaybe (entryDescription entry) (normalizeDescription maybeDescription),
        entryIdentity = normalizeIdentity maybeIdentity <|> entryIdentity entry,
        entryMeta = normalizeMetadata maybeMetadata <|> entryMeta entry,
        entryTimestamp
      }
  where
    normalizeDescription :: Maybe Description -> Maybe Description
    normalizeDescription (Just (MkDescription "")) = Nothing
    normalizeDescription d = d

    normalizeIdentity :: Maybe Identity -> Maybe Identity
    normalizeIdentity (Just (MkIdentity "")) = Nothing
    normalizeIdentity i = i

    normalizeMetadata :: Maybe Metadata -> Maybe Metadata
    normalizeMetadata (Just (MkMetadata "")) = Nothing
    normalizeMetadata m = m

updateCiphertext ::
  (MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadTime m) =>
  Bool ->
  Entry ->
  m Entry
updateCiphertext True entry = do
  input <- prompt "Enter text to encrypt: "
  entryTimestamp <- now
  keyId <- configKeyId <$> askConfig
  entryCiphertext <- encrypt keyId $ mkPlaintext input
  return $ updateEntry entry {entryTimestamp, entryCiphertext}
updateCiphertext False entry =
  return entry

modify ::
  (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m, MonadTime m) =>
  Target ->
  Bool ->
  Maybe Identity ->
  Maybe Metadata ->
  m Response
modify target modifyPlaintext maybeIdentity maybeMetadata = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> do
      timestamp <- now
      let modifiedEntry = update Nothing maybeIdentity maybeMetadata entry timestamp
      updated <- updateCiphertext modifyPlaintext modifiedEntry
      put updated
      delete entry
      return Modified
    [] -> throwAmbiguousInput "There are no entries matching your input criteria."
    _ -> throwAmbiguousInput "There are multiple entries matching your input criteria."

remove :: (MonadAppError m, MonadStore m) => Target -> m Response
remove target = do
  entries <- runQuery $ queryFromTarget target
  case entries of
    [entry] -> delete entry >> return Removed
    _ -> throwAmbiguousInput "There are multiple entries matching your input criteria."

check :: (MonadAppError m, MonadConfigReader m, MonadStore m) => m Response
check = do
  keyId <- configKeyId <$> askConfig
  totalCount <- getCount
  keyCount <- getCountOf keyId
  if totalCount == keyCount
    then return CheckedForMultipleKeys
    else throwDefault "All entries do not have the same keyid"

importJSON :: (MonadAppError m, MonadFilesystem m, MonadStore m) => FilePath -> m Response
importJSON jsonFile = do
  input <- readFileBytes jsonFile
  entries <- maybe (throwDefault ("unable to decode " ++ jsonFile)) return (Aeson.decode input :: Maybe [Entry])
  mapM_ put entries
  return Imported

exportJSON :: (MonadFilesystem m, MonadStore m) => FilePath -> m Response
exportJSON jsonFile = do
  entries <- selectAll
  let aesonConfig = AesonPretty.defConfig {AesonPretty.confCompare = AesonPretty.keyOrder entryKeyOrder}
      output = AesonPretty.encodePretty' aesonConfig . List.sort $ entries
  writeFileBytes jsonFile output
  return Exported

eval ::
  ( MonadAppError m,
    MonadConfigReader m,
    MonadEncrypt m,
    MonadFilesystem m,
    MonadInteraction m,
    MonadStore m,
    MonadTime m
  ) =>
  Command ->
  m Response
eval Add {addDescription, addIdentity, addMeta} =
  checkKey $ add addDescription addIdentity addMeta
eval Lookup {lookupDescription, lookupIdentity, lookupVerbose} =
  lookup lookupDescription lookupIdentity lookupVerbose
eval Modify {modifyTarget, modifyPlaintext, modifyIdentity, modifyMeta} =
  checkKey $ modify modifyTarget modifyPlaintext modifyIdentity modifyMeta
eval Remove {removeTarget} =
  remove removeTarget
eval CheckForMultipleKeys =
  check
eval Import {importFile} =
  importJSON importFile
eval Export {exportFile} =
  exportJSON exportFile

showVersion :: String
showVersion = Version.showVersion Paths_ananke.version
