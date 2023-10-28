module Main (main) where

import Ananke (configure, run)
import System.Exit qualified as Exit

main :: IO ()
main = configure >>= run >>= Exit.exitWith
