{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ananke.Orphans () where

import Ananke.Data (Description (..), Identity (..), Metadata (..), Plaintext (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink xs = Text.pack <$> shrink (Text.unpack xs)

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
