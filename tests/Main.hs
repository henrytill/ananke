module Main (main) where

import           Control.Monad
import           System.Exit
import qualified Test.QuickCheck   as QC

import           Hecate.Properties


-- Included for backwards compatibility
isSuccess :: QC.Result -> Bool
isSuccess QC.Success{} = True
isSuccess _            = False

main :: IO ()
main = do
  rs <- doProperties
  unless (all isSuccess rs) exitFailure
