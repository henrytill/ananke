module Hecate.Backend.JSON.AppState
  ( EntriesMap
  , AppState (..)
  , mkAppState
  , put
  , delete
  , runQuery
  , selectAll
  , getCount
  , getCountOfKeyId
  ) where

import           Control.Arrow ((&&&))
import qualified Data.Maybe    as Maybe
import           Data.Multimap (Multimap)
import qualified Data.Multimap as Multimap
import           Data.Set      (Set)
import qualified Data.Set      as Set
import qualified Data.Text     as Text

import           Hecate.Data


type EntriesMap = Multimap Description Entry

data AppState = MkAppState
  { appStateDirty :: Bool
  , appStateData  :: EntriesMap
  } deriving (Show, Eq)

mkAppState :: [Entry] -> AppState
mkAppState = MkAppState False . Multimap.fromList . fmap (entryDescription &&& id)

update :: (EntriesMap -> EntriesMap) -> AppState -> AppState
update f  = MkAppState True . f . appStateData

put    :: Entry -> AppState -> AppState
delete :: Entry -> AppState -> AppState
put    entry = update (Multimap.insert (entryDescription entry) entry)
delete entry = update (Multimap.delete (entryDescription entry) entry)

idenIsInfixOf :: Identity    -> Identity    -> Bool
descIsInfixOf :: Description -> Description -> Bool
idenIsInfixOf (MkIdentity    needle) (MkIdentity    haystack) = Text.isInfixOf needle haystack
descIsInfixOf (MkDescription needle) (MkDescription haystack) = Text.isInfixOf needle haystack

idenMatcher :: Maybe Identity -> Maybe Identity -> Bool
idenMatcher queryIden entryIden = Maybe.fromMaybe True (idenIsInfixOf <$> queryIden <*> entryIden)

filterEntries :: (Maybe Identity -> Bool) -> Set Entry -> [Entry]
filterEntries predicate = Set.foldr f []
  where
    f entry acc | predicate $ entryIdentity entry = entry : acc
                | otherwise                       = acc

queryFolder :: Query -> Description -> Set Entry -> [Entry] -> [Entry]
queryFolder (MkQuery (Just qid) Nothing Nothing Nothing) _ entries acc =
  Set.toList matches ++ acc
  where
    matches = Set.filter (\entry -> entryId entry == qid) entries
queryFolder query description entries acc =
  case descMatches of
    Just True -> filterEntries idenMatches entries ++ acc
    _         -> acc
  where
    descMatches = descIsInfixOf <$> queryDescription query <*> pure description
    idenMatches = idenMatcher $ queryIdentity query

runQuery :: Query -> AppState -> [Entry]
runQuery query appState = Multimap.foldrWithKey (queryFolder query) [] (appStateData appState)

selectAll :: AppState -> [Entry]
selectAll = Multimap.elems . appStateData

getCount :: AppState -> Int
getCount = Multimap.size . appStateData

getCountOfKeyId :: KeyId -> AppState -> Int
getCountOfKeyId keyId = length . filter (\entry -> entryKeyId entry == keyId) . selectAll
