module Hecate.Error (module Hecate.Error) where

import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)
import           TOML              (TOMLError)


-- | 'AppError' represents application errors
data AppError
  = CsvDecoding String
  | TOML TOMLError
  | Configuration String
  | Aeson String
  | GPG String
  | Database String
  | FileSystem String
  | AmbiguousInput String
  | Migration String
  | Default String
  deriving (Typeable)

instance Show AppError where
  show (CsvDecoding s)    = "CSV Decoding Error: " ++ s
  show (TOML e)           = show e
  show (Configuration s)  = "Configuration Error: " ++ s
  show (Aeson s)          = "Aeson Error: " ++ s
  show (GPG s)            = s
  show (Database s)       = "Database Error: " ++ s
  show (FileSystem s)     = "Filesystem Error: " ++ s
  show (AmbiguousInput s) = "Ambiguous Input: " ++ s
  show (Migration s)      = "Migration Error: " ++ s
  show (Default s)        = "Error: " ++ s

instance Exception AppError
