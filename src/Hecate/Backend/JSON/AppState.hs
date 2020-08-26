{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Backend.JSON.AppState
  ( EntriesMap
  , AppState (..)
  , HasAppState (..)
  , mkAppState
  , put
  , delete
  , query
  , selectAll
  , getCount
  , getCountOfKeyId
  ) where

import qualified Data.Maybe             as Maybe
import           Data.Multimap          (Multimap)
import qualified Data.Multimap          as Multimap
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Lens.Family2
import           Lens.Family2.Unchecked (lens)

import           Hecate.Data            hiding (query)


type EntriesMap = Multimap Description Entry

data AppState = AppState
  { _appStateDirty :: Bool
  , _appStateData  :: EntriesMap
  } deriving (Show, Eq)

class HasAppState t where
  appState      :: Lens' t AppState
  appStateDirty :: Lens' t Bool
  appStateData  :: Lens' t EntriesMap
  appStateDirty = appState . appStateDirty
  appStateData  = appState . appStateData
  {-# INLINE appStateDirty #-}
  {-# INLINE appStateData  #-}

instance HasAppState AppState where
  appState      = id
  appStateDirty = lens _appStateDirty (\ s v -> s{_appStateDirty = v})
  appStateData  = lens _appStateData  (\ s v -> s{_appStateData  = v})
  {-# INLINE appState      #-}
  {-# INLINE appStateDirty #-}
  {-# INLINE appStateData  #-}

mkAppState :: [Entry] -> AppState
mkAppState entries = AppState
  { _appStateDirty = False
  , _appStateData  = Multimap.fromList (tupler <$> entries)
  }
  where
    tupler ent = (ent ^. entryDescription, ent)

update :: (EntriesMap -> EntriesMap) -> AppState -> AppState
update f state = state{_appStateDirty = True, _appStateData = updated}
  where
    updated = f (_appStateData state)

put    :: Entry -> AppState -> AppState
delete :: Entry -> AppState -> AppState
put    ent = update (Multimap.insert (_entryDescription ent) ent)
delete ent = update (Multimap.delete (_entryDescription ent) ent)

idenIsInfixOf :: Identity    -> Identity    -> Bool
descIsInfixOf :: Description -> Description -> Bool
idenIsInfixOf (Identity    needle) (Identity    haystack) = Text.isInfixOf needle haystack
descIsInfixOf (Description needle) (Description haystack) = Text.isInfixOf needle haystack

idenMatcher :: Maybe Identity    -> Maybe Identity -> Bool
idenMatcher queryIden entryIden = Maybe.fromMaybe True (idenIsInfixOf <$> queryIden <*> entryIden)

filterEntries :: (Maybe Identity -> Bool) -> Set Entry -> [Entry]
filterEntries predicate = Set.foldr f []
  where
    f e acc | predicate (_entryIdentity e) = e : acc
            | otherwise                    = acc

queryFolder :: Query -> Description -> Set Entry -> [Entry] -> [Entry]
queryFolder q d es acc | Just True <- descMatches = filterEntries idenMatches es ++ acc
                       | otherwise                = acc
  where
    descMatches = descIsInfixOf <$> _queryDescription q <*> pure d
    idenMatches = idenMatcher (_queryIdentity q)

query :: Query -> AppState -> [Entry]
query q AppState{_appStateData} = Multimap.foldrWithKey (queryFolder q) [] _appStateData

selectAll :: AppState -> [Entry]
selectAll = Multimap.elems . _appStateData

getCount :: AppState -> Int
getCount = Multimap.size . _appStateData

getCountOfKeyId :: KeyId -> AppState -> Int
getCountOfKeyId keyid = length . filter (\ e -> _entryKeyId e == keyid) . selectAll
