{-# LANGUAGE FlexibleContexts #-}

module IOProperties (ioTests) where

import Control.Monad
import Control.Monad.Except
import Data.Monoid
import Hecate.Crypto
import Hecate.IO
import Hecate.Types
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath (takeDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Instances()

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

genHex :: Gen Char
genHex = elements $ ['A'..'F'] ++ ['0'..'9']

prop_roundTripAuthFile :: Property
prop_roundTripAuthFile = monadicIO $ do
  fp  <- pick $ ((<>) "/tmp/hecate-tests/testFile_") <$> sequence (take 8 (repeat genHex))
  mp  <- pick $ (MasterPassword . T.pack)            <$> listOf1 arbitrary
  s   <- pick $ (Salt . ByteString64 . C.pack)       <$> sequence (take 12 (repeat arbitrary))
  ret <- run  $ runExceptT $ roundTripAuthFile fp mp s
  assert (ret == (Right True))

roundTripEntries
  :: (MonadIO m, MonadError Error m)
  => MasterPassword
  -> Salt
  -> Description
  -> Maybe Identity
  -> PlainText
  -> Maybe Metadata
  -> m Bool
roundTripEntries mp s d u pt m = do
  mk  <- pure $ generateMasterKey mp s
  e   <- entry mk d u pt m
  rpt <- getCipherText mk e
  return (pt == rpt)

prop_roundTripEntries :: Property
prop_roundTripEntries = monadicIO $ do
  mp  <- pick $ (MasterPassword . T.pack)      <$> listOf1 arbitrary
  s   <- pick $ (Salt . ByteString64 . C.pack) <$> sequence (take 12 (repeat arbitrary))
  d   <- pick $ arbitrary
  u   <- pick $ arbitrary
  pt  <- pick $ arbitrary
  mt  <- pick $ arbitrary
  ret <- run  $ runExceptT $ roundTripEntries mp s d u pt mt
  assert (ret == (Right True))

ioTests :: [Property]
ioTests = [ prop_roundTripAuthFile
          , prop_roundTripEntries
          ]
