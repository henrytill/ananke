{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import CryptoProperties
import IOProperties
import System.Exit
import Test.QuickCheck

tests :: [Property]
tests = cryptoTests ++ ioTests

doProperties :: [Property] -> IO Bool
doProperties ps =
  mapM (quickCheckWithResult stdArgs) ps >>= pure . all isSuccess
  where
    isSuccess :: Result -> Bool
    isSuccess (Success _ _ _) = True
    isSuccess _               = False

main :: IO ()
main = do
  result <- doProperties tests
  unless result exitFailure
