module Data.Multimap.Tests
  ( runTests
  ) where

import qualified Data.Multimap as Multimap
import qualified Data.Set      as Set
import           Test.Dwergaz


testKey1 :: String
testKey1 = "one"

testKey2 :: String
testKey2 = "two"

empty :: Multimap.Multimap String Int
empty = Multimap.empty

inserter :: (Ord k, Ord v) => (k, v) -> Multimap.Multimap k v -> Multimap.Multimap k v
inserter (k, v) = Multimap.insert k v

emptyIsNull :: Test
emptyIsNull =
  Predicate "null returns true on empty map"
            Multimap.null
            empty

nonEmptyIsNotNull :: Test
nonEmptyIsNotNull =
  Predicate "null returns false on non-empty map"
            (not . Multimap.null)
            (Multimap.insert testKey1 1 empty)

nonEmptyHasItem :: Test
nonEmptyHasItem =
  Expect "non-empty map has inserted item"
         (==)
         (Multimap.lookup testKey1 testMap)
         (Set.fromList [1])
  where
    testMap = foldr inserter empty [(testKey1, 1)]

twoValuesUnderKey :: Test
twoValuesUnderKey =
  Expect "two values can be stored under the same key"
         (==)
         (Multimap.lookup testKey1 testMap)
         (Set.fromList [1, 2])
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

deleteAllWorks :: Test
deleteAllWorks =
  Expect "deleteAll removes all values under a key"
         (==)
         (Multimap.lookup testKey1 (Multimap.deleteAll testKey1 testMap))
         Set.empty
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

deleteWorks :: Test
deleteWorks =
  Expect "delete removes a specific value under a key"
         (==)
         (Multimap.lookup testKey1 (Multimap.delete testKey1 1 testMap))
         (Set.fromList [2])
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2)]

fromListWorks :: Test
fromListWorks =
  Expect "fromList constructs a map from a list of pairs"
         (==)
         (Multimap.fromList    [(testKey1, 1), (testKey1, 2)])
         (foldr inserter empty [(testKey1, 1), (testKey1, 2)])

elemsWorks :: Test
elemsWorks =
  Expect "elems returns all values in the map"
         (==)
         (Multimap.elems testMap)
         [1, 2, 3]
  where
    testMap = foldr inserter empty [(testKey1, 1), (testKey1, 2), (testKey2, 3)]

tests :: [Test]
tests =
  [ emptyIsNull
  , nonEmptyIsNotNull
  , nonEmptyHasItem
  , twoValuesUnderKey
  , deleteAllWorks
  , deleteWorks
  , fromListWorks
  , elemsWorks
  ]

runTests :: [Result]
runTests = runTest <$> tests
