module Hecate.Error (module Hecate.Error) where

-- | 'AppError' represents application errors
data AppError
  = CsvDecoding String
  | TomlParsing String
  | GPG String
  | FileSystem String
  | AmbiguousInput String
  | Default String
  deriving Eq

instance Show AppError where
  show (CsvDecoding s)    = "CSV Decoding Error: " ++ s
  show (TomlParsing s)    = "TOML Parsing Error: " ++ s
  show (GPG s)            = "GPG Error: " ++ s
  show (FileSystem s)     = "Filesystem Error: " ++ s
  show (AmbiguousInput s) = "Ambiguous Input: " ++ s
  show (Default s)        = "Error: " ++ s
