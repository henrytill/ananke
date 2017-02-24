module Main where

import Control.Monad
import CryptoProperties
import DatabaseProperties
import IOProperties
import System.Exit
import Test.QuickCheck

props :: [Property]
props = cryptoTests ++ ioTests

isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

doProperties :: [Property] -> IO [Result]
doProperties = mapM (quickCheckWithResult stdArgs)

main :: IO ()
main = do
  rs1 <- doProperties props
  rs2 <- doDatabaseProperties
  unless (all isSuccess $ rs1 ++ rs2) exitFailure
