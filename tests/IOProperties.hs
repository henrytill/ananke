{-# LANGUAGE FlexibleContexts #-}

module IOProperties (ioTests) where

import Control.Monad.Except
import Data.Monoid
import Hecate.Crypto
import Hecate.IO
import Hecate.Types
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath (takeDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Generators
import Instances ()

roundTripAuthFile
  :: (MonadIO m, MonadError Error m)
  => FilePath
  -> MasterPassword
  -> Salt
  -> m Bool
roundTripAuthFile authFile mp s = do
  let path = takeDirectory authFile
  dirExists <- liftIO $ doesDirectoryExist path
  unless dirExists (liftIO $ createDirectory path)
  auth      <- pure (genAuth mp s)
  _         <- writeAuthFile authFile auth
  bs        <- readAuthFile authFile
  fileAuth  <- parseAuth bs
  return (fileAuth == auth)

prop_roundTripAuthFile :: Property
prop_roundTripAuthFile = monadicIO $ do
  fp  <- pick $ ("/tmp/hecate-tests/testFile_" <>) <$> replicateM 8 genHex
  mp  <- pick arbitrary
  s   <- pick arbitrary
  ret <- run  $ runExceptT $ roundTripAuthFile fp mp s
  assert (ret == Right True)

roundTripEntries
  :: (MonadIO m, MonadError Error m)
  => MasterKey
  -> Description
  -> Maybe Identity
  -> PlainText
  -> Maybe Metadata
  -> m Bool
roundTripEntries mk d u pt m = do
  e   <- entry mk d u pt m
  rpt <- getCipherText mk e
  return (pt == rpt)

prop_roundTripEntries :: Property
prop_roundTripEntries = monadicIO $ do
  mk  <- pick genMasterKey
  d   <- pick arbitrary
  u   <- pick arbitrary
  pt  <- pick arbitrary
  mt  <- pick arbitrary
  ret <- run $ runExceptT $ roundTripEntries mk d u pt mt
  assert (ret == Right True)

ioTests :: [Property]
ioTests = [ prop_roundTripAuthFile
          , prop_roundTripEntries
          ]
