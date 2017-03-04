module Main where

import Control.Monad
import Hecate.Database.Properties
import System.Exit
import Test.QuickCheck

isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

main :: IO ()
main = do
  rs <- doDatabaseProperties
  unless (all isSuccess rs) exitFailure
