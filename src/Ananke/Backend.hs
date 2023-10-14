module Ananke.Backend
  ( currentSchemaVersion
  , createSchemaFile
  , getSchemaVersion
  ) where

import qualified Data.ByteString.Lazy.Char8 as Char8

import           Ananke.Class               (MonadFilesystem (..))
import           Ananke.Data


currentSchemaVersion :: SchemaVersion
currentSchemaVersion = MkSchemaVersion 3

getSchemaVersionFromFile :: MonadFilesystem m => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = MkSchemaVersion . read . Char8.unpack <$> readFileBytes path

createSchemaFile :: MonadFilesystem m => FilePath -> SchemaVersion -> m SchemaVersion
createSchemaFile path version = writeFileBytes path (Char8.pack . show $ version) >> return version

getSchemaVersion :: MonadFilesystem m => FilePath -> m SchemaVersion
getSchemaVersion path = do
  fileExists <- doesFileExist path
  if fileExists
    then getSchemaVersionFromFile path
    else createSchemaFile path currentSchemaVersion
