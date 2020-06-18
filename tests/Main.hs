module Main (main) where

import qualified Control.Monad     as Monad
import qualified System.Exit       as Exit
import qualified Test.QuickCheck   as QC

import qualified Hecate.Properties as Properties


-- Included for backwards compatibility
isSuccess :: QC.Result -> Bool
isSuccess QC.Success{} = True
isSuccess _            = False

main :: IO ()
main = do
  rs <- Properties.doProperties
  Monad.unless (all isSuccess rs) Exit.exitFailure
