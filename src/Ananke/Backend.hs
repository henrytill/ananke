module Ananke.Backend
  ( currentSchemaVersion,
    createSchemaFile,
    getSchemaVersion,
    mkMigrateMessage,
  )
where

import Ananke.Class (MonadFilesystem (..))
import Ananke.Data
import Data.ByteString.Lazy.Char8 qualified as Char8

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = MkSchemaVersion 3

getSchemaVersionFromFile :: (MonadFilesystem m) => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = MkSchemaVersion . read . Char8.unpack <$> readFileBytes path

createSchemaFile :: (MonadFilesystem m) => FilePath -> SchemaVersion -> m SchemaVersion
createSchemaFile path version = writeFileBytes path (Char8.pack . show $ version) >> return version

getSchemaVersion :: (MonadFilesystem m) => FilePath -> m SchemaVersion
getSchemaVersion path = do
  fileExists <- doesFileExist path
  if fileExists
    then getSchemaVersionFromFile path
    else createSchemaFile path currentSchemaVersion

mkMigrateMessage :: SchemaVersion -> SchemaVersion -> String
mkMigrateMessage previous current =
  "Migrating database from schema version "
    ++ show previous
    ++ " to version "
    ++ show current
    ++ "..."
