module Main (main) where

import Ananke (configure, run)
import qualified System.Exit as Exit

main :: IO ()
main = configure >>= run >>= Exit.exitWith
