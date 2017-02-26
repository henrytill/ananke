{-# LANGUAGE FlexibleContexts #-}

module Hecate.IO.Properties (ioTests) where

import Control.Monad.Except
import Data.Monoid
import Hecate.Crypto
import Hecate.IO
import Hecate.Generators
import Hecate.Orphans ()
import Hecate.Types
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath (takeDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic

roundTripAuthFile
  :: (MonadIO m, MonadError AppError m)
  => FilePath
  -> MasterPassword
  -> Salt
  -> m Bool
roundTripAuthFile authFile mp s = do
  let path = takeDirectory authFile
  dirExists <- liftIO $ doesDirectoryExist path
  unless dirExists (liftIO $ createDirectory path)
  mk       <- pure $ generateMasterKey mp s
  auth     <- genAuth mk s
  _        <- writeAuthFile authFile auth
  bs       <- readAuthFile authFile
  fileAuth <- parseAuth bs
  _        <- ensureAuth mp fileAuth
  return (fileAuth == auth)

prop_roundTripAuthFile :: Property
prop_roundTripAuthFile = monadicIO $ do
  fp  <- pick $ ("/tmp/hecate-tests/testFile_" <>) <$> replicateM 8 genHex
  mp  <- pick arbitrary
  s   <- pick arbitrary
  ret <- run  $ runExceptT $ roundTripAuthFile fp mp s
  assert (ret == Right True)

roundTripEntries
  :: (MonadIO m, MonadError AppError m)
  => MasterKey
  -> Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Bool
roundTripEntries mk d u pt m = do
  e   <- entry mk d u pt m
  rpt <- getPlainText mk e
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
