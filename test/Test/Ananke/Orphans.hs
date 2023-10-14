{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ananke.Orphans () where

import qualified Data.Text       as T
import           Test.QuickCheck (Arbitrary (..))

import           Ananke.Data     (Description (..), Identity (..), Metadata (..), Plaintext (..))


instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary Description where
  arbitrary                 = MkDescription <$> arbitrary
  shrink (MkDescription xs) = MkDescription <$> shrink xs

instance Arbitrary Identity where
  arbitrary              = MkIdentity <$> arbitrary
  shrink (MkIdentity xs) = MkIdentity <$> shrink xs

instance Arbitrary Plaintext where
  arbitrary               = MkPlaintext <$> arbitrary
  shrink (MkPlaintext xs) = MkPlaintext <$> shrink xs

instance Arbitrary Metadata where
  arbitrary              = MkMetadata <$> arbitrary
  shrink (MkMetadata xs) = MkMetadata <$> shrink xs
