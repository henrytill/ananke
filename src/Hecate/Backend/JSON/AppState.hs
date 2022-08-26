{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Backend.JSON.AppState
  ( EntriesMap
  , AppState (..)
  , mkAppState
  , put
  , delete
  , query
  , selectAll
  , getCount
  , getCountOfKeyId
  ) where

import qualified Data.Maybe    as Maybe
import           Data.Multimap (Multimap)
import qualified Data.Multimap as Multimap
import           Data.Set      (Set)
import qualified Data.Set      as Set
import qualified Data.Text     as Text

import           Hecate.Data   hiding (query)


type EntriesMap = Multimap Description Entry

data AppState = MkAppState
  { appStateDirty :: Bool
  , appStateData  :: EntriesMap
  } deriving (Show, Eq)

mkAppState :: [Entry] -> AppState
mkAppState entries = MkAppState
  { appStateDirty = False
  , appStateData  = Multimap.fromList (tupler <$> entries)
  }
  where
    tupler ent = (entryDescription ent, ent)

update :: (EntriesMap -> EntriesMap) -> AppState -> AppState
update f state = state{appStateDirty = True, appStateData = updated}
  where
    updated = f (appStateData state)

put    :: Entry -> AppState -> AppState
delete :: Entry -> AppState -> AppState
put    ent = update (Multimap.insert (entryDescription ent) ent)
delete ent = update (Multimap.delete (entryDescription ent) ent)

idenIsInfixOf :: Identity    -> Identity    -> Bool
descIsInfixOf :: Description -> Description -> Bool
idenIsInfixOf (MkIdentity    needle) (MkIdentity    haystack) = Text.isInfixOf needle haystack
descIsInfixOf (MkDescription needle) (MkDescription haystack) = Text.isInfixOf needle haystack

idenMatcher :: Maybe Identity    -> Maybe Identity -> Bool
idenMatcher queryIden entryIden = Maybe.fromMaybe True (idenIsInfixOf <$> queryIden <*> entryIden)

filterEntries :: (Maybe Identity -> Bool) -> Set Entry -> [Entry]
filterEntries predicate = Set.foldr f []
  where
    f e acc | predicate (entryIdentity e) = e : acc
            | otherwise                   = acc

queryFolder :: Query -> Description -> Set Entry -> [Entry] -> [Entry]
queryFolder (MkQuery (Just eid) Nothing Nothing Nothing) _ es acc =
  Set.toList matches ++ acc
  where
    matches = Set.filter (\e -> entryId e == eid) es
queryFolder q d es acc
  | Just True <- descMatches = filterEntries idenMatches es ++ acc
  | otherwise                = acc
  where
    descMatches = descIsInfixOf <$> queryDescription q <*> pure d
    idenMatches = idenMatcher (queryIdentity q)

query :: Query -> AppState -> [Entry]
query q MkAppState{appStateData} = Multimap.foldrWithKey (queryFolder q) [] appStateData

selectAll :: AppState -> [Entry]
selectAll = Multimap.elems . appStateData

getCount :: AppState -> Int
getCount = Multimap.size . appStateData

getCountOfKeyId :: KeyId -> AppState -> Int
getCountOfKeyId keyid = length . filter (\e -> entryKeyId e == keyid) . selectAll
