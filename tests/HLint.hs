module Main where

import Control.Monad
import Language.Haskell.HLint3
import System.Environment
import System.Exit

main :: IO ()
main = do
  args  <- getArgs
  hints <- hlint $ ["src", "executables"] ++ args
  unless (null hints) exitFailure
