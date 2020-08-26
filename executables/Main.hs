module Main (main) where

import qualified System.Exit as Exit

import           Hecate      (configure, run)


main :: IO ()
main = configure >>= run >>= Exit.exitWith
