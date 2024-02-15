module Main (main) where

import Ananke (configure, parseCommand, run)
import System.Exit qualified as Exit

main :: IO ()
main = do
  cmd <- parseCommand
  cfg <- configure
  ret <- run cmd cfg
  Exit.exitWith ret
