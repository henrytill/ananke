{-# LANGUAGE OverloadedStrings #-}

module Hecate.Properties
  ( doProperties
  ) where

import qualified Control.Monad           as Monad
import           Data.List               ((\\))
import           Data.Monoid             (First (..))
import qualified Data.Text               as T
import           Data.Text.Arbitrary     ()
import           Database.SQLite.Simple  hiding (Error)
import           Lens.Family2
import qualified System.Directory        as Directory
import qualified System.IO.Temp          as Temp
import           Test.QuickCheck         (Arbitrary (..), Property, Result)
import qualified Test.QuickCheck         as QuickCheck
import qualified Test.QuickCheck.Monadic as Monadic
import qualified Text.Printf             as Printf

import           Hecate.Backend.SQLite   (AppContext, HasAppContext (..))
import qualified Hecate.Backend.SQLite   as SQLite
import qualified Hecate.Configuration    as Configuration
import           Hecate.Data
import qualified Hecate.Evaluator        as Evaluator
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
  :: (MonadAppError m, MonadInteraction m, MonadEncrypt m, MonadConfigReader m)
  => TestData
  -> m Entry
createEntryFromTestData td = do
  cfg       <- askConfig
  timestamp <- now
  createEntry encrypt
              (cfg ^. configKeyId)
              timestamp
              (_testDescription td)
              (_testIdentity td)
              (_testPlaintext td)
              (_testMetadata td)

addEntryToDatabase
  :: ( MonadAppError m
     , MonadInteraction m
     , MonadEncrypt m
     , MonadStore m
     , MonadConfigReader m
     )
  => [TestData]
  -> m [Entry]
addEntryToDatabase tds = do
  es <- mapM createEntryFromTestData tds
  mapM_ put es
  return es

createFilePath :: AppContext -> Int -> FilePath
createFilePath ctx x
  = ctx ^. configDataDirectory ++ "/export-" ++ Printf.printf "%05d" x ++ ".csv"

isNotEmpty :: TestData -> Bool
isNotEmpty testData
  = _testDescription testData /= Description    T.empty  &&
    _testIdentity    testData /= Just (Identity T.empty) &&
    _testPlaintext   testData /= Plaintext      T.empty  &&
    _testMetadata    testData /= Just (Metadata T.empty)

entriesHaveSameContent :: (MonadAppError m, MonadEncrypt m) => Entry -> Entry -> m Bool
entriesHaveSameContent e1 e2 = do
  plaintext1 <- decrypt (_entryCiphertext e1)
  plaintext2 <- decrypt (_entryCiphertext e2)
  return ((_entryDescription e1 == _entryDescription e2) &&
          (_entryIdentity    e1 == _entryIdentity    e2) &&
          (_entryMeta        e1 == _entryMeta        e2) &&
          (plaintext1           == plaintext2))

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = Monadic.monadicIO $ do
  tds <- Monadic.pick (QuickCheck.listOf1 arbitrary)
  es  <- Monadic.run (SQLite.run (addEntryToDatabase tds) ctx)
  res <- Monadic.run (SQLite.run selectAll ctx)
  Monadic.assert (null (es \\ res))

prop_roundTripEntriesToCSV :: AppContext -> Property
prop_roundTripEntriesToCSV ctx = Monadic.monadicIO $ do
  tds  <- Monadic.pick (QuickCheck.listOf1 (QuickCheck.suchThat arbitrary isNotEmpty))
  x    <- Monadic.pick (QuickCheck.suchThat arbitrary (> 0))
  let file = createFilePath ctx x
  es   <- Monadic.run (SQLite.run (mapM createEntryFromTestData tds) ctx)
  _    <- Monadic.run (Evaluator.exportCSV file es)
  ies  <- Monadic.run (SQLite.run (Evaluator.importCSV file) ctx)
  bs   <- Monadic.run (Monad.zipWithM entriesHaveSameContent es ies)
  Monadic.assert (and bs)

tests :: [AppContext -> Property]
tests =
  [ prop_roundTripEntriesToDatabase
  , prop_roundTripEntriesToCSV
  ]

doProperties :: IO [Result]
doProperties = do
  sysTempDir <- Temp.getCanonicalTemporaryDirectory
  dir        <- Temp.createTempDirectory sysTempDir "hecate"
  _          <- print ("dir: " ++ dir)
  let preConfig = PreConfig (First (Just dir)) mempty mempty mempty
  _          <- Directory.copyFile "./example/hecate.toml" (dir ++ "/hecate.toml")
  ctx        <- Configuration.configureWith preConfig >>= SQLite.initialize
  _          <- SQLite.run Evaluator.setup ctx
  results    <- mapM (\ p -> QuickCheck.quickCheckWithResult QuickCheck.stdArgs (p ctx)) tests
  _          <- close (ctx ^. appContextConnection)
  return results
