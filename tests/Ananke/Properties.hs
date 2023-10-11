{-# LANGUAGE OverloadedStrings #-}

module Ananke.Properties
  ( doProperties
  ) where

import           Data.List               ((\\))
import           Data.Monoid             (First (..))
import qualified Database.SQLite3        as SQLite3
import qualified System.Directory        as Directory
import qualified System.IO.Temp          as Temp
import           Test.QuickCheck         (Arbitrary (..), Property, Result)
import qualified Test.QuickCheck         as QuickCheck
import qualified Test.QuickCheck.Monadic as Monadic

import           Ananke.Backend.SQLite   (AppContext (..))
import qualified Ananke.Backend.SQLite   as SQLite
import qualified Ananke.Configuration    as Configuration
import           Ananke.Data
import           Ananke.Interfaces
import           Ananke.Orphans          ()


data TestData = MkTestData
  { testDescription :: Description
  , testIdentity    :: Maybe Identity
  , testPlaintext   :: Plaintext
  , testMetadata    :: Maybe Metadata
  } deriving (Eq, Show)

instance Arbitrary TestData where
  arbitrary                       = MkTestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (MkTestData as bs cs ds) = MkTestData <$> shrink as <*> shrink bs <*> shrink cs <*> shrink ds

createEntryFromTestData
  :: (MonadAppError m, MonadInteraction m, MonadEncrypt m, MonadConfigReader m)
  => TestData
  -> m Entry
createEntryFromTestData td = do
  cfg       <- askConfig
  timestamp <- now
  let keyId = configKeyId cfg
  ciphertext <- encrypt keyId $ testPlaintext td
  return $ mkEntry keyId
                   timestamp
                   (testDescription td)
                   (testIdentity td)
                   ciphertext
                   (testMetadata td)

addEntryToDatabase
  :: (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m)
  => [TestData]
  -> m [Entry]
addEntryToDatabase tds = do
  es <- mapM createEntryFromTestData tds
  mapM_ put es
  return es

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = Monadic.monadicIO $ do
  tds <- Monadic.pick $ QuickCheck.listOf1 arbitrary
  es  <- Monadic.run  $ SQLite.run (addEntryToDatabase tds) ctx
  res <- Monadic.run  $ SQLite.run selectAll ctx
  Monadic.assert . null $ es \\ res

tests :: [AppContext -> Property]
tests =
  [ prop_roundTripEntriesToDatabase
  ]

doProperties :: IO [Result]
doProperties = do
  sysTempDir <- Temp.getCanonicalTemporaryDirectory
  dir        <- Temp.createTempDirectory sysTempDir "ananke"
  _          <- print ("dir: " ++ dir)
  let preConfig = MkPreConfig (First (Just dir)) mempty mempty mempty
  _          <- Directory.copyFile "./example/ananke.conf" (dir ++ "/ananke.conf")
  ctx        <- Configuration.configureWith preConfig >>= SQLite.initialize
  _          <- SQLite.run SQLite.setup ctx
  results    <- mapM (\p -> QuickCheck.quickCheckWithResult QuickCheck.stdArgs (p ctx)) tests
  _          <- SQLite3.close (appContextDatabase ctx)
  return results
