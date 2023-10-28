module Main (main) where

import Control.Monad (unless)
import System.Exit qualified as Exit
import Test.Ananke.Backend.JSON qualified as JSON
import Test.Ananke.Properties qualified as Properties
import Test.Data.Multimap qualified as Multimap
import Test.Dwergaz qualified as Dwergaz
import Test.QuickCheck qualified as QC

-- Included for backwards compatibility
isSuccess :: QC.Result -> Bool
isSuccess QC.Success {} = True
isSuccess _ = False

exitOnFalse :: Bool -> IO ()
exitOnFalse = flip unless Exit.exitFailure

main :: IO ()
main = do
  -- Multimap tests
  let multimapResults = Multimap.runTests
  mapM_ print multimapResults
  exitOnFalse $ all Dwergaz.isPassed multimapResults
  -- JSON tests
  jsonResults <- JSON.runTests
  mapM_ print jsonResults
  exitOnFalse $ all Dwergaz.isPassed jsonResults
  -- Property tests
  propertyResults <- Properties.doProperties
  exitOnFalse $ all isSuccess propertyResults
