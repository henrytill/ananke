{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ananke.Orphans () where

import Ananke.Data (Description (..), Identity (..), Metadata (..), Plaintext (..))
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary Description where
  arbitrary = MkDescription <$> arbitrary
  shrink (MkDescription d) = MkDescription <$> shrink d

instance Arbitrary Identity where
  arbitrary = MkIdentity <$> arbitrary
  shrink (MkIdentity i) = MkIdentity <$> shrink i

instance Arbitrary Plaintext where
  arbitrary = MkPlaintext <$> arbitrary
  shrink (MkPlaintext p) = MkPlaintext <$> shrink p

instance Arbitrary Metadata where
  arbitrary = MkMetadata <$> arbitrary
  shrink (MkMetadata m) = MkMetadata <$> shrink m
