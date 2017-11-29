module Main (main) where

import           Control.Monad           (unless)
import           Language.Haskell.HLint3 (hlint)
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)


main :: IO ()
main = do
  args  <- getArgs
  hints <- hlint (["src", "executables", "tests"] ++ args)
  unless (null hints) exitFailure
