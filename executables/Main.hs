module Main (main) where

import qualified System.Exit as Exit

import           Ananke      (configure, run)


main :: IO ()
main = configure >>= run >>= Exit.exitWith
