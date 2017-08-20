{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Database.Properties
  ( doDatabaseProperties
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List               ((\\))
import Data.Text.Arbitrary     ()
import Database.SQLite.Simple  hiding (Error)
import System.Directory        (copyFile)
import System.Posix.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Hecate.Context
import Hecate.Data
import Hecate.Database
import Hecate.Error
import Hecate.GPG              (Plaintext)
import Hecate.Orphans          ()


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
  es <- mapM (\ td -> createEntry (testDescription td) (testIdentity td) (testPlainText td) (testMetadata td)) tds
  _  <- mapM (put c) es
  return es

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = monadicIO $ do
  tds <- pick $ listOf1 arbitrary
  es  <- run $ runExceptT $ flip runReaderT ctx $ addEntryToDatabase (appContextConnection ctx) tds
  res <- run $ selectAll (appContextConnection ctx)
  case es of
    Right xs -> assert $ null (xs \\ res)
    _        -> assert False

dbTests :: [AppContext -> Property]
dbTests = [ prop_roundTripEntriesToDatabase ]

doDatabaseProperties :: IO [Result]
doDatabaseProperties = do
  dir         <- mkdtemp "/tmp/hecate-tests-"
  _           <- copyFile "./example/hecate.toml" (dir ++ "/hecate.toml")
  (Right ctx) <- runExceptT (configure dir >>= createContext)
  results     <- mapM (\ p -> quickCheckWithResult stdArgs (p ctx)) dbTests
  _           <- close (appContextConnection ctx)
  return results
