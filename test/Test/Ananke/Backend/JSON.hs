{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ananke.Backend.JSON (runTests) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Test.Dwergaz

import           Ananke.Backend.JSON  (encodeJSON)

dataFile :: FilePath
dataFile = "example" ++ "/db" ++ "/data.json"

roundtripJson :: IO Test
roundtripJson = do
  input <- BSL.readFile dataFile
  let maybeOutput = encodeJSON <$> Aeson.decode input
  return $ Expect "json to roundtrip" (==) maybeOutput (Just input)

tests :: [IO Test]
tests = [ roundtripJson ]

runTests :: IO [Result]
runTests =  traverse (fmap runTest) tests
