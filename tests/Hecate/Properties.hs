{-# LANGUAGE OverloadedStrings #-}

module Hecate.Properties
  ( doProperties
  ) where

import           Control.Monad           (zipWithM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List               ((\\))
import qualified Data.Text               as T
import           Data.Text.Arbitrary     ()
import           Database.SQLite.Simple  hiding (Error)
import           Lens.Family2
import           System.Directory        (copyFile)
import           System.Posix.Temp
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Printf             (printf)

import           Hecate.Context
import           Hecate.Data
import           Hecate.Database
import           Hecate.Evaluator        (importCSV, exportCSV)
import           Hecate.GPG              (Plaintext(..), decrypt)
import           Hecate.Orphans          ()


data TestData = TestData
  { _testDescription :: Description
  , _testIdentity    :: Maybe Identity
  , _testPlaintext   :: Plaintext
  , _testMetadata    :: Maybe Metadata
  } deriving (Eq, Show)

instance Arbitrary TestData where
  arbitrary                     = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (TestData as bs cs ds) = TestData <$> shrink as <*> shrink bs <*> shrink cs <*> shrink ds

createEntries
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => [TestData]
  -> m [Entry]
createEntries = mapM k where
  k td = createEntry (_testDescription td) (_testIdentity td) (_testPlaintext td) (_testMetadata td)

addEntryToDatabase
  :: (MonadIO m, MonadReader r m, HasAppContext r)
  => Connection
  -> [TestData]
  -> m [Entry]
addEntryToDatabase c tds = do
  es <- createEntries tds
  _  <- mapM (put c) es
  return es

createFilePath :: AppContext -> Int -> FilePath
createFilePath ctx x
  = ctx ^. configDataDirectory ++ "/export-" ++ printf "%05d" x ++ ".csv"

isNotEmpty :: TestData -> Bool
isNotEmpty testData
  = _testDescription testData /= Description    T.empty  &&
    _testIdentity    testData /= Just (Identity T.empty) &&
    _testPlaintext   testData /= Plaintext      T.empty  &&
    _testMetadata    testData /= Just (Metadata T.empty)

entriesHaveSameContent :: MonadIO m => Entry -> Entry -> m Bool
entriesHaveSameContent e1 e2 = do
  plaintext1 <- decrypt (_entryCiphertext e1)
  plaintext2 <- decrypt (_entryCiphertext e2)
  return ((_entryDescription e1 == _entryDescription e2) &&
          (_entryIdentity    e1 == _entryIdentity    e2) &&
          (_entryMeta        e1 == _entryMeta        e2) &&
          (plaintext1           == plaintext2))

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = monadicIO $ do
  tds <- pick (listOf1 arbitrary)
  es  <- run (runReaderT (addEntryToDatabase conn tds) ctx)
  res <- run (selectAll conn)
  assert (null (es \\ res))
  where
    conn = ctx ^. appContextConnection

prop_roundTripEntriesToCSV :: AppContext -> Property
prop_roundTripEntriesToCSV ctx = monadicIO $ do
  tds  <- pick (listOf1 (suchThat arbitrary isNotEmpty))
  x    <- pick (suchThat arbitrary (> 0))
  file <- pure (createFilePath ctx x)
  es   <- run (runReaderT (createEntries tds) ctx)
  _    <- run (exportCSV file es)
  ies  <- run (runReaderT (importCSV file) ctx)
  bs   <- run (zipWithM entriesHaveSameContent es ies)
  assert (and bs)

tests :: [AppContext -> Property]
tests =
  [ prop_roundTripEntriesToDatabase
  , prop_roundTripEntriesToCSV
  ]

doProperties :: IO [Result]
doProperties = do
  dir     <- mkdtemp "/tmp/hecate-tests-"
  _       <- copyFile "./example/hecate.toml" (dir ++ "/hecate.toml")
  ctx     <- configure dir >>= createContext
  _       <- runReaderT setup ctx
  results <- mapM (\ p -> quickCheckWithResult stdArgs (p ctx)) tests
  _       <- close (ctx ^. appContextConnection)
  return results
