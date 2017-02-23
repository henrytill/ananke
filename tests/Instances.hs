{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Hecate.Types
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Text as T

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance Arbitrary Description where
  arbitrary               = Description <$> arbitrary
  shrink (Description xs) = Description <$> shrink xs

instance Arbitrary Identity where
  arbitrary            = Identity <$> arbitrary
  shrink (Identity xs) = Identity <$> shrink xs

instance Arbitrary PlainText where
  arbitrary             = PlainText <$> arbitrary
  shrink (PlainText xs) = PlainText <$> shrink xs

instance Arbitrary Metadata where
  arbitrary            = Metadata <$> arbitrary
  shrink (Metadata xs) = Metadata <$> shrink xs
