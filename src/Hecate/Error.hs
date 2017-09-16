module Hecate.Error (module Hecate.Error) where

import Control.Exception
import Data.Typeable

-- | 'AppError' represents application errors
data AppError
  = CsvDecoding String
  | TomlParsing String
  | GPG String
  | FileSystem String
  | AmbiguousInput String
  | MigrationError String
  | CheckError String
  | Default String
  deriving (Eq, Typeable)

instance Show AppError where
  show (CsvDecoding s)    = "CSV Decoding Error: " ++ s
  show (TomlParsing s)    = "TOML Parsing Error: " ++ s
  show (GPG s)            = "GPG Error: " ++ s
  show (FileSystem s)     = "Filesystem Error: " ++ s
  show (AmbiguousInput s) = "Ambiguous Input: " ++ s
  show (MigrationError s) = "Migration Error: " ++ s
  show (CheckError s)     = "Check Error: " ++ s
  show (Default s)        = "Error: " ++ s

instance Exception AppError
