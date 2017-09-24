{-# LANGUAGE OverloadedStrings #-}

module Hecate.Database.Properties
  ( doDatabaseProperties
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List               ((\\))
import Data.Text.Arbitrary     ()
import Database.SQLite.Simple  hiding (Error)
import Lens.Simple             hiding (Identity)
import System.Directory        (copyFile)
import System.Posix.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Hecate.Context
import Hecate.Data
import Hecate.Database
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
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Connection
  -> [TestData]
  -> m [Entry]
addEntryToDatabase c tds = do
  es <- mapM (\ td -> createEntry (testDescription td) (testIdentity td) (testPlainText td) (testMetadata td)) tds
  _  <- mapM (put c) es
  return es

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = monadicIO $ do
  tds <- pick (listOf1 arbitrary)
  es  <- run (runReaderT (addEntryToDatabase conn tds) ctx)
  res <- run (selectAll conn)
  assert (null (es \\ res))
  where
    conn = ctx ^. appContextConnection

dbTests :: [AppContext -> Property]
dbTests = [ prop_roundTripEntriesToDatabase ]

doDatabaseProperties :: IO [Result]
doDatabaseProperties = do
  dir         <- mkdtemp "/tmp/hecate-tests-"
  _           <- copyFile "./example/hecate.toml" (dir ++ "/hecate.toml")
  ctx         <- configure dir >>= createContext
  _           <- runReaderT setup ctx
  results     <- mapM (\ p -> quickCheckWithResult stdArgs (p ctx)) dbTests
  _           <- close (ctx ^. appContextConnection)
  return results
