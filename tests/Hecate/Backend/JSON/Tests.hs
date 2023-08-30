{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hecate.Backend.JSON.Tests (runTests) where

import           Control.Monad        (liftM)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Test.Dwergaz

import           Hecate.Backend.JSON  (encodeJSON)

dataFile :: FilePath
dataFile = "example" ++ "/db" ++ "/data.json"

roundtripJson :: IO Test
roundtripJson = do
  input <- BSL.readFile dataFile
  let Just output = encodeJSON <$> Aeson.decode input
  return $ Expect "json to roundtrip" (==) output input

tests :: [IO Test]
tests = [ roundtripJson ]

runTests :: IO [Result]
runTests =  traverse (liftM runTest) tests
