module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

import Hecate.Database.Properties

isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

main :: IO ()
main = do
  rs <- doDatabaseProperties
  unless (all isSuccess rs) exitFailure
