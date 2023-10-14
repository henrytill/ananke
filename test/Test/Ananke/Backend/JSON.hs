{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Ananke.Backend.JSON (runTests) where

import           Control.Monad.State        (MonadState, StateT, gets, modify, runStateT)
import           Control.Monad.Trans        (lift)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Test.Dwergaz

import           Ananke.Backend.JSON        (encodeJSON, preInitialize)
import           Ananke.Class
import           Ananke.Data
import           Ananke.Error


data TestState = MkTestState
  { testStateInput   :: BSL.ByteString
  , testSchemaInput  :: BSL.ByteString
  , testStateOutput  :: Maybe BSL.ByteString
  , testSchemaOutput :: Maybe BSL.ByteString
  } deriving (Show)

mkTestState :: BSL.ByteString -> SchemaVersion -> TestState
mkTestState input schemaVersion = MkTestState
  { testStateInput   = input
  , testSchemaInput  = Char8.pack . show $ schemaVersion
  , testStateOutput  = Nothing
  , testSchemaOutput = Nothing
  }

newtype TestMigrate a = MkTestMigrate { unTestMigrate :: StateT TestState (Either AppError) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState TestState
           )

runTestMigrate :: TestState -> TestMigrate a -> Either AppError (a, TestState)
runTestMigrate state m = runStateT (unTestMigrate m) state

err :: AppError -> TestMigrate a
ok  :: a        -> TestMigrate a
err = MkTestMigrate . lift . Left
ok  = MkTestMigrate . lift . Right

instance MonadAppError TestMigrate where
  configurationError  = err . Configuration
  gpgError            = err . GPG
  databaseError       = err . Database
  filesystemError     = err . Filesystem
  ambiguousInputError = err . AmbiguousInput
  migrationError      = err . Migration
  defaultError        = err . Default

instance MonadFilesystem TestMigrate where
  doesFileExist "data/db/data.json" = ok True
  doesFileExist "data/db/schema"    = ok True
  doesFileExist _                   = ok False

  doesDirectoryExist "data"    = ok True
  doesDirectoryExist "data/db" = ok True
  doesDirectoryExist _         = ok True

  createDirectory _ = ok ()

  readFileBytes "data/db/data.json" = gets testStateInput
  readFileBytes "data/db/schema"    = gets testSchemaInput
  readFileBytes path                = err . Filesystem $ path ++ " does not exist"

  writeFileBytes "data/db/data.json" bytes = modify $ \s -> s{testStateOutput = Just bytes}
  writeFileBytes "data/db/schema"    bytes = modify $ \s -> s{testSchemaOutput = Just bytes}
  writeFileBytes path                _     = err . Filesystem $ path ++ " does not exist"

instance MonadInteraction TestMigrate where
  message _ = ok ()
  prompt  _ = ok []

config :: Config
config = MkConfig
  { configDataDirectory     = "data"
  , configBackend           = JSON
  , configKeyId             = MkKeyId "371C136C"
  , configAllowMultipleKeys = False
  }

migrateV3 :: IO Test
migrateV3 = do
  input    <- BSL.readFile $ "test" ++ "/data-schema-v2.json"
  expected <- BSL.readFile $ "test" ++ "/data-schema-v3.json"
  result   <- return . go . mkTestState input $ MkSchemaVersion 2
  case result of
    Left failure  -> error . show $ failure
    Right success -> return $ Expect "migration to schema version 3 to succeed" (==) (Just expected) (testStateOutput success)
  where
    go :: TestState -> Either AppError TestState
    go state = fmap snd . runTestMigrate state . preInitialize config $ MkSchemaVersion 3

roundtripJson :: IO Test
roundtripJson = do
  input <- BSL.readFile "example/db/data.json"
  let maybeOutput = encodeJSON <$> Aeson.decode input
  return $ Expect "json to roundtrip" (==) maybeOutput (Just input)

tests :: [IO Test]
tests =
  [ migrateV3
  , roundtripJson
  ]

runTests :: IO [Result]
runTests =  traverse (fmap runTest) tests
