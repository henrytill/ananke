module Main (main) where

import Control.Monad
import System.Exit
import Test.QuickCheck

import Hecate.Properties

isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

main :: IO ()
main = do
  rs <- doProperties
  unless (all isSuccess rs) exitFailure
