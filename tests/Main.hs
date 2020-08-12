module Main (main) where

import qualified Control.Monad       as Monad
import qualified System.Exit         as Exit
import qualified Test.Dwergaz        as Dwergaz
import qualified Test.QuickCheck     as QC

import qualified Data.Multimap.Tests as Multimap
import qualified Hecate.Properties   as Properties


-- Included for backwards compatibility
isSuccess :: QC.Result -> Bool
isSuccess QC.Success{} = True
isSuccess _            = False

exitOnFalse :: Bool -> IO ()
exitOnFalse = flip Monad.unless Exit.exitFailure

main :: IO ()
main = do
  -- Multimap tests
  let multimapResults = Multimap.runTests
  exitOnFalse $ all Dwergaz.isPassed multimapResults
  -- Property tests
  propertyResults <- Properties.doProperties
  exitOnFalse $ all isSuccess propertyResults
