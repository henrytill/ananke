{-# LANGUAGE OverloadedStrings #-}

module Hecate.Properties
  ( doProperties
  ) where

import           Control.Monad           (zipWithM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List               ((\\))
import           Data.Monoid             (First(..))
import qualified Data.Text               as T
import           Data.Text.Arbitrary     ()
import           Database.SQLite.Simple  hiding (Error)
import           Lens.Family2
import           System.Directory        (copyFile)
import           System.Posix.Temp
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Printf             (printf)

import           Hecate.Carriers         (runAppM)
import           Hecate.Configuration
import           Hecate.Data
import           Hecate.Evaluator        (importCSV, exportCSV, setup)
import           Hecate.GPG              (Plaintext(..))
import           Hecate.Interfaces
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

createEntryFromTestData
  :: (MonadThrow m, MonadInteraction m, MonadEncrypt m, MonadReader r m, HasAppContext r)
  => TestData
  -> m Entry
createEntryFromTestData td = do
  ctx       <- ask
  timestamp <- now
  createEntry encrypt
              (ctx ^. configKeyId)
              timestamp
              (_testDescription td)
              (_testIdentity td)
              (_testPlaintext td)
              (_testMetadata td)

addEntryToDatabase
  :: ( MonadThrow m
     , MonadInteraction m
     , MonadEncrypt m
     , MonadStore m
     , MonadReader r m
     , HasAppContext r
     )
  => [TestData]
  -> m [Entry]
addEntryToDatabase tds = do
  es <- mapM createEntryFromTestData tds
  _  <- mapM_ put es
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

entriesHaveSameContent :: (MonadThrow m, MonadIO m, MonadEncrypt m) => Entry -> Entry -> m Bool
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
  es  <- run (runAppM (addEntryToDatabase tds) ctx)
  res <- run (runAppM selectAll ctx)
  assert (null (es \\ res))

prop_roundTripEntriesToCSV :: AppContext -> Property
prop_roundTripEntriesToCSV ctx = monadicIO $ do
  tds  <- pick (listOf1 (suchThat arbitrary isNotEmpty))
  x    <- pick (suchThat arbitrary (> 0))
  file <- pure (createFilePath ctx x)
  es   <- run (runAppM (mapM createEntryFromTestData tds) ctx)
  _    <- run (exportCSV file es)
  ies  <- run (runAppM (importCSV file) ctx)
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
  let preConfig = PreConfig (First (Just dir)) mempty mempty
  _       <- copyFile "./example/hecate.toml" (dir ++ "/hecate.toml")
  ctx     <- configureWith preConfig >>= createContext
  _       <- runAppM setup ctx
  results <- mapM (\ p -> quickCheckWithResult stdArgs (p ctx)) tests
  _       <- close (ctx ^. appContextConnection)
  return results
