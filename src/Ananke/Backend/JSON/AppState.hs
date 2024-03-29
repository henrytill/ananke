module Ananke.Backend.JSON.AppState
  ( EntriesMap,
    AppState (..),
    mkAppState,
    put,
    delete,
    runQuery,
    selectAll,
    getCount,
    getCountOf,
  )
where

import Ananke.Data
import Control.Arrow ((&&&))
import Data.Maybe qualified as Maybe
import Data.Multimap (Multimap)
import Data.Multimap qualified as Multimap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

type EntriesMap = Multimap Description Entry

data AppState = MkAppState
  { appStateDirty :: Bool,
    appStateData :: EntriesMap
  }
  deriving (Show, Eq)

mkAppState :: [Entry] -> AppState
mkAppState = MkAppState False . Multimap.fromList . fmap (entryDescription &&& id)

update :: (EntriesMap -> EntriesMap) -> AppState -> AppState
update f = MkAppState True . f . appStateData

put :: Entry -> AppState -> AppState
put = update . (Multimap.insert <$> entryDescription <*> id)

delete :: Entry -> AppState -> AppState
delete = update . (Multimap.delete <$> entryDescription <*> id)

idenIsInfixOf :: Identity -> Identity -> Bool
idenIsInfixOf (MkIdentity needle) (MkIdentity haystack) = Text.isInfixOf needle haystack

descIsInfixOf :: Description -> Description -> Bool
descIsInfixOf (MkDescription needle) (MkDescription haystack) = Text.isInfixOf needle haystack

idenMatcher :: Maybe Identity -> Maybe Identity -> Bool
idenMatcher queryIden entryIden = Maybe.fromMaybe True (idenIsInfixOf <$> queryIden <*> entryIden)

filterEntries :: (Maybe Identity -> Bool) -> Set Entry -> [Entry]
filterEntries predicate = Set.foldr f []
  where
    f entry acc
      | predicate $ entryIdentity entry = entry : acc
      | otherwise = acc

queryFolder :: Query -> Description -> Set Entry -> [Entry] -> [Entry]
queryFolder (MkQuery (Just qid) Nothing Nothing Nothing) _ entries acc =
  Set.toList matches ++ acc
  where
    matches = Set.filter (\entry -> entryId entry == qid) entries
queryFolder query description entries acc =
  case descMatches of
    Just True -> filterEntries idenMatches entries ++ acc
    _ -> acc
  where
    descMatches :: Maybe Bool
    descMatches = descIsInfixOf <$> queryDescription query <*> pure description

    idenMatches :: Maybe Identity -> Bool
    idenMatches = idenMatcher $ queryIdentity query

runQuery :: Query -> AppState -> [Entry]
runQuery query appState = Multimap.foldrWithKey (queryFolder query) [] (appStateData appState)

selectAll :: AppState -> [Entry]
selectAll = Multimap.elems . appStateData

getCount :: AppState -> Int
getCount = Multimap.size . appStateData

getCountOf :: KeyId -> AppState -> Int
getCountOf keyId = length . filter (\entry -> entryKeyId entry == keyId) . selectAll
