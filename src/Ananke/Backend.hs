module Ananke.Backend
  ( currentSchemaVersion
  , createSchemaFile
  , getSchemaVersion
  ) where

import qualified System.Directory as Directory

import           Ananke.Data

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = MkSchemaVersion 2

getSchemaVersionFromFile :: FilePath -> IO SchemaVersion
getSchemaVersionFromFile path = MkSchemaVersion . read <$> readFile path

createSchemaFile :: FilePath -> SchemaVersion -> IO SchemaVersion
createSchemaFile path version = writeFile path (show version) >> return version

getSchemaVersion :: FilePath -> IO SchemaVersion
getSchemaVersion path = do
  fileExists <- Directory.doesFileExist path
  if fileExists
    then getSchemaVersionFromFile path
    else createSchemaFile path currentSchemaVersion
