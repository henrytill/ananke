{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ananke.Backend.JSON (runTests) where

import Ananke.Backend.JSON (encodeJSON, preInitialize)
import Ananke.Class
import Ananke.Data
import Ananke.Error
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import Control.Monad.Trans (lift)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as Char8
import System.FilePath ((<.>), (</>))
import Test.Dwergaz

data TestState = MkTestState
  { testStateInput :: ByteString,
    testSchemaInput :: ByteString,
    testStateOutput :: Maybe ByteString,
    testSchemaOutput :: Maybe ByteString
  }
  deriving (Show)

mkTestState :: ByteString -> SchemaVersion -> TestState
mkTestState input schemaVersion =
  MkTestState
    { testStateInput = input,
      testSchemaInput = Char8.pack . show $ schemaVersion,
      testStateOutput = Nothing,
      testSchemaOutput = Nothing
    }

newtype TestMigrate a = MkTestMigrate {unTestMigrate :: StateT TestState (Either AppError) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState TestState
    )

runTestMigrate :: TestState -> TestMigrate a -> Either AppError (a, TestState)
runTestMigrate state m = runStateT (unTestMigrate m) state

err :: AppError -> TestMigrate a
err = MkTestMigrate . lift . Left

ok :: a -> TestMigrate a
ok = MkTestMigrate . lift . Right

instance MonadAppError TestMigrate where
  throwConfiguration = err . Configuration
  throwGPG = err . GPG
  throwDatabase = err . Database
  throwFilesystem = err . Filesystem
  throwAmbiguousInput = err . AmbiguousInput
  throwMigration = err . Migration
  throwDefault = err . Default

dataDir :: FilePath
dataDir = "data"

dbDir :: FilePath
dbDir = dataDir </> "db"

dataJsonFile :: FilePath
dataJsonFile = dbDir </> "data" <.> "json"

schemaFile :: FilePath
schemaFile = dbDir </> "schema"

instance MonadFilesystem TestMigrate where
  doesFileExist path
    | path == dataJsonFile = ok True
    | path == schemaFile = ok True
    | otherwise = ok False

  doesDirExist path
    | path == dataDir = ok True
    | path == dbDir = ok True
    | otherwise = ok True

  createDir _ = ok ()

  readFileText _ = error "readFileText is not implemented"

  readFileBytes path
    | path == dataJsonFile = gets testStateInput
    | path == schemaFile = gets testSchemaInput
    | otherwise = err . Filesystem $ path ++ " does not exist"

  writeFileBytes path bytes
    | path == dataJsonFile = modify $ \s -> s {testStateOutput = Just bytes}
    | path == schemaFile = modify $ \s -> s {testSchemaOutput = Just bytes}
    | otherwise = err . Filesystem $ path ++ " does not exist"

instance MonadInteraction TestMigrate where
  message _ = ok ()
  prompt _ = ok []

config :: Config
config =
  MkConfig
    { configDir = "data",
      configDataDir = "data",
      configBackend = JSON,
      configKeyId = MkKeyId "371C136C",
      configMultKeys = False
    }

migrateV3 :: IO Test
migrateV3 = do
  input <- ByteString.readFile $ "test" </> "data-schema-v2" <.> "json"
  expected <- ByteString.readFile $ "test" </> "data-schema-v3" <.> "json"
  result <- return . go . mkTestState input $ MkSchemaVersion 2
  case result of
    Left failure -> error . show $ failure
    Right success -> return $ Expect "migration to schema version 3 to succeed" (==) (Just expected) (testStateOutput success)
  where
    go :: TestState -> Either AppError TestState
    go state = fmap snd . runTestMigrate state . preInitialize config $ MkSchemaVersion 3

roundtripJson :: IO Test
roundtripJson = do
  input <- ByteString.readFile $ "example" </> "db" </> "data" <.> "json"
  let maybeOutput = encodeJSON <$> Aeson.decode input
  return $ Expect "json to roundtrip" (==) maybeOutput (Just input)

tests :: [IO Test]
tests =
  [ migrateV3,
    roundtripJson
  ]

runTests :: IO [Result]
runTests = traverse (fmap runTest) tests
