{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hecate.Orphans where

import Data.Text.Arbitrary ()
import Test.QuickCheck

import Hecate.Data
import Hecate.GPG          (Plaintext(..))

{-# ANN module "HLint: ignore Use module export list" #-}

instance Arbitrary Description where
  arbitrary               = Description <$> arbitrary
  shrink (Description xs) = Description <$> shrink xs

instance Arbitrary Identity where
  arbitrary            = Identity <$> arbitrary
  shrink (Identity xs) = Identity <$> shrink xs

instance Arbitrary Plaintext where
  arbitrary             = Plaintext <$> arbitrary
  shrink (Plaintext xs) = Plaintext <$> shrink xs

instance Arbitrary Metadata where
  arbitrary            = Metadata <$> arbitrary
  shrink (Metadata xs) = Metadata <$> shrink xs
