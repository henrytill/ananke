{-# LANGUAGE FlexibleContexts #-}

module Hecate.Database.Properties (doDatabaseProperties) where

import Control.Monad.Except
import Data.List ((\\))
import Database.SQLite.Simple hiding (Error)
import Hecate.IO
import Hecate.Types
import Hecate.Generators
import Hecate.Orphans ()
import Hecate.Database
import Test.QuickCheck
import Test.QuickCheck.Monadic

data TestData = TestData
  { test_description :: Description
  , test_identity    :: Maybe Identity
  , test_plainText   :: PlainText
  , test_metadata    :: Maybe Metadata
  } deriving (Eq, Show)

instance Arbitrary TestData where
  arbitrary                     = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (TestData as bs cs ds) = TestData <$> shrink as <*> shrink bs <*> shrink cs <*> shrink ds

addEntryToDatabase
  :: (MonadIO m, MonadError AppError m)
  => Connection
  -> MasterKey
  -> [TestData]
  -> m [Entry]
addEntryToDatabase c mk tds = do
  es <- mapM (\td -> entry mk (test_description td) (test_identity td) (test_plainText td) (test_metadata td)) tds
  _  <- mapM (put c) es
  return es

prop_roundTripEntriesToDatabase :: MasterKey -> Connection -> Property
prop_roundTripEntriesToDatabase mk c = monadicIO $ do
  tds <- pick $ listOf1 arbitrary
  es  <- run $ runExceptT $ addEntryToDatabase c mk tds
  res <- run $ selectAll c
  case es of
    Right xs -> assert $ null (xs \\ res)
    _        -> assert False

dbTests :: [MasterKey -> Connection -> Property]
dbTests = [ prop_roundTripEntriesToDatabase ]

doDatabaseProperties :: IO [Result]
doDatabaseProperties = do
  c       <- open "/tmp/hecate-tests/test.db"
  _       <- initDatabase c
  mk      <- generate genMasterKey
  results <- mapM (\p -> quickCheckWithResult stdArgs (p mk c)) dbTests
  _       <- close c
  return results
