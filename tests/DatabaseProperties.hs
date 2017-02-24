{-# LANGUAGE FlexibleContexts #-}

module DatabaseProperties (doDatabaseProperties) where

import Control.Monad.Except
import Data.List ((\\))
import Database.SQLite.Simple hiding (Error)
import Hecate.Database
import Hecate.IO
import Hecate.Types
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Generators
import Instances ()

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
  :: (MonadIO m, MonadError Error m)
  => Connection
  -> MasterKey
  -> [TestData]
  -> m [Entry]
addEntryToDatabase conn mk tds = do
  es <- mapM (\td ->entry mk (test_description td) (test_identity td) (test_plainText td) (test_metadata td)) tds
  _  <- mapM (insert conn) es
  return es

prop_roundTripEntriesToDatabase :: MasterKey -> Connection -> Property
prop_roundTripEntriesToDatabase mk conn = monadicIO $ do
  tds <- pick $ listOf1 arbitrary
  es  <- run $ runExceptT $ addEntryToDatabase conn mk tds
  res <- run $ getAll conn
  case es of
    Right xs -> assert $ null (xs \\ res)
    _        -> assert False

dbTests :: [MasterKey -> Connection -> Property]
dbTests = [ prop_roundTripEntriesToDatabase ]

doDatabaseProperties :: IO [Result]
doDatabaseProperties = do
  conn    <- open "/tmp/hecate-tests/test.db"
  _       <- initDatabase conn
  mk      <- generate genMasterKey
  results <- mapM (\p -> quickCheckWithResult stdArgs (p mk conn)) dbTests
  _       <- close conn
  return results
