module Main (main) where

import qualified Control.Monad          as Monad
import qualified Language.Haskell.HLint as HLint
import qualified System.Environment     as Env
import qualified System.Exit            as Exit


main :: IO ()
main = do
  args  <- Env.getArgs
  hints <- HLint.hlint (["src", "executables", "tests"] ++ args)
  Monad.unless (null hints) Exit.exitFailure
