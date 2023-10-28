{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ananke.Properties
  ( doProperties,
  )
where

import Ananke.Backend (currentSchemaVersion)
import Ananke.Backend.SQLite (AppContext (..))
import Ananke.Backend.SQLite qualified as SQLite
import Ananke.Class
import Ananke.Configuration qualified as Configuration
import Ananke.Data
import Data.List ((\\))
import Data.Monoid (First (..))
import Database.SQLite3 qualified as SQLite3
import System.Directory qualified as Directory
import System.IO.Temp qualified as Temp
import Test.Ananke.Orphans ()
import Test.QuickCheck (Arbitrary (..), Property, Result)
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Monadic qualified as Monadic

data Datum = MkDatum
  { datumDescription :: Description,
    datumIdentity :: Maybe Identity,
    datumPlaintext :: Plaintext,
    datumMetadata :: Maybe Metadata
  }
  deriving (Eq, Show)

instance Arbitrary Datum where
  arbitrary = MkDatum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (MkDatum a b c d) = MkDatum <$> shrink a <*> shrink b <*> shrink c <*> shrink d

createEntry ::
  (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadTime m) =>
  Datum ->
  m Entry
createEntry d = do
  cfg <- askConfig
  timestamp <- now
  let keyId = configKeyId cfg
  ciphertext <- encrypt keyId $ datumPlaintext d
  return $ mkEntry keyId timestamp (datumDescription d) (datumIdentity d) ciphertext (datumMetadata d)

addEntryToDatabase ::
  (MonadAppError m, MonadConfigReader m, MonadEncrypt m, MonadInteraction m, MonadStore m, MonadTime m) =>
  [Datum] ->
  m [Entry]
addEntryToDatabase ds = do
  es <- mapM createEntry ds
  mapM_ put es
  return es

prop_roundTripEntriesToDatabase :: AppContext -> Property
prop_roundTripEntriesToDatabase ctx = Monadic.monadicIO $ do
  ds <- Monadic.pick $ QuickCheck.listOf1 arbitrary
  es <- Monadic.run $ SQLite.run (addEntryToDatabase ds) ctx
  rs <- Monadic.run $ SQLite.run selectAll ctx
  Monadic.assert . null $ es \\ rs

tests :: [AppContext -> Property]
tests = [prop_roundTripEntriesToDatabase]

doProperties :: IO [Result]
doProperties = do
  tmp <- Temp.getCanonicalTemporaryDirectory
  dir <- Temp.createTempDirectory tmp "ananke"
  print ("dir: " ++ dir)
  _ <- Directory.copyFile "./example/ananke.ini" (dir ++ "/ananke.ini")
  let preConfigDir = First (Just dir)
      preConfigDataDir = First (Just dir)
      preConfigBackend = First (Just SQLite)
      preConfig = mempty {preConfigDir, preConfigDataDir, preConfigBackend}
  ctx <- Configuration.configureWith preConfig >>= SQLite.initialize
  _ <- SQLite.run (SQLite.setup currentSchemaVersion) ctx
  rs <- mapM (\p -> QuickCheck.quickCheckWithResult QuickCheck.stdArgs (p ctx)) tests
  _ <- SQLite3.close (appContextDatabase ctx)
  return rs
