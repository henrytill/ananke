{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Database.Properties (doDatabaseProperties) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List ((\\))
import Database.SQLite.Simple hiding (Error)
import Data.Text.Arbitrary ()
import Hecate.IO
import Hecate.Types
import Hecate.Orphans ()
import Hecate.Database
import System.Directory (createDirectory)
import System.Posix.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic

data TestData = TestData
  { testDescription :: Description
  , testIdentity    :: Maybe Identity
  , testPlainText   :: Plaintext
  , testMetadata    :: Maybe Metadata
  } deriving (Eq, Show)

instance Arbitrary TestData where
  arbitrary                     = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (TestData as bs cs ds) = TestData <$> shrink as <*> shrink bs <*> shrink cs <*> shrink ds

addEntryToDatabase
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Connection
  -> [TestData]
  -> m [Entry]
addEntryToDatabase c tds = do
  es <- mapM (\td -> entry (testDescription td) (testIdentity td) (testPlainText td) (testMetadata td)) tds
  _  <- mapM (put c) es
  return es

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = monadicIO $ do
  tds <- pick $ listOf1 arbitrary
  es  <- run $ runExceptT $ flip runReaderT ctx $ addEntryToDatabase (_conn ctx) tds
  res <- run $ selectAll (_conn ctx)
  case es of
    Right xs -> assert $ null (xs \\ res)
    _        -> assert False

dbTests :: [AppContext -> Property]
dbTests = [ prop_roundTripEntriesToDatabase ]

doDatabaseProperties :: IO [Result]
doDatabaseProperties = do
  dir           <- mkdtemp "/tmp/hecate-tests-"
  _             <- createDirectory (dir ++ "/db")
  c             <- open (dir ++ "/db/db.sqlite")
  schemaVersion <- getSchemaVersion (dir ++ "/db/schema")
  _             <- runExceptT $ initDatabase c schemaVersion
  ctx           <- pure $ AppContext (Fingerprint "371C136C") c
  results       <- mapM (\p -> quickCheckWithResult stdArgs (p ctx)) dbTests
  _             <- close c
  return results
