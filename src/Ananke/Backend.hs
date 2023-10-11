module Ananke.Backend
  ( currentSchemaVersion
  , createSchemaFile
  , getSchemaVersion
  ) where

import           Ananke.Data
import           Ananke.Interfaces

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = MkSchemaVersion 2

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
