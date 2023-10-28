module Main (main) where

import Control.Monad (unless)
import qualified System.Exit as Exit
import qualified Test.Ananke.Backend.JSON as JSON
import qualified Test.Ananke.Properties as Properties
import qualified Test.Data.Multimap as Multimap
import qualified Test.Dwergaz as Dwergaz
import qualified Test.QuickCheck as QC

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
