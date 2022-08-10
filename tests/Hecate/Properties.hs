{-# LANGUAGE OverloadedStrings #-}

module Hecate.Properties
  ( doProperties
  ) where

import           Data.List                   ((\\))
import           Data.Monoid                 (First (..))
import           Database.SQLite.Simple      hiding (Error)
import qualified System.Directory            as Directory
import qualified System.IO.Temp              as Temp
import           Test.QuickCheck             (Arbitrary (..), Property, Result)
import qualified Test.QuickCheck             as QuickCheck
import qualified Test.QuickCheck.Monadic     as Monadic

import           Hecate.Backend.SQLiteSimple (AppContext (..))
import qualified Hecate.Backend.SQLiteSimple as SQLite
import qualified Hecate.Configuration        as Configuration
import           Hecate.Data
import qualified Hecate.Evaluator            as Evaluator
import           Hecate.Interfaces
import           Hecate.Orphans              ()


data TestData = TestData
  { testDescription :: Description
  , testIdentity    :: Maybe Identity
  , testPlaintext   :: Plaintext
  , testMetadata    :: Maybe Metadata
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
              (configKeyId cfg)
              timestamp
              (testDescription td)
              (testIdentity td)
              (testPlaintext td)
              (testMetadata td)

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

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = Monadic.monadicIO $ do
  tds <- Monadic.pick (QuickCheck.listOf1 arbitrary)
  es  <- Monadic.run (SQLite.run (addEntryToDatabase tds) ctx)
  res <- Monadic.run (SQLite.run selectAll ctx)
  Monadic.assert (null (es \\ res))

tests :: [AppContext -> Property]
tests =
  [ prop_roundTripEntriesToDatabase
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
  _          <- close (appContextConnection ctx)
  return results
