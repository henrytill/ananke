module Data.Multimap
  ( Multimap
  , empty
  , null
  , insert
  , deleteAll
  , delete
  , lookup
  , fromList
  , elems
  ) where

import           Prelude    hiding (lookup, null)

import           Data.Map   (Map)
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import           Data.Set   (Set)
import qualified Data.Set   as Set


newtype Multimap k v = Multimap { unMultimap :: Map k (Set v) }
  deriving (Eq, Ord, Show)

empty :: Multimap k v
empty = Multimap Map.empty

null :: Multimap k v -> Bool
null (Multimap m) = Map.null m

inserter :: Ord v => v -> Maybe (Set v) -> Maybe (Set v)
inserter v Nothing  = Just (Set.singleton v)
inserter v (Just s) = Just (Set.insert v s)

insert :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
insert k v (Multimap m) = Multimap $ Map.alter (inserter v) k m

deleteAll :: Ord k => k -> Multimap k v -> Multimap k v
deleteAll k (Multimap m) = Multimap $ Map.delete k m

deleter :: Ord v => v -> Maybe (Set v) -> Maybe (Set v)
deleter _ Nothing  = Nothing
deleter v (Just s) =
  let
    updated = Set.delete v s
  in
    if Set.null updated
    then Nothing
    else Just updated

delete :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
delete k v (Multimap m) = Multimap $ Map.alter (deleter v) k m

lookup :: Ord k => k -> Multimap k v -> Set v
lookup k (Multimap m) = Maybe.fromMaybe Set.empty (Map.lookup k m)

fromList :: (Ord k, Ord v) => [(k, v)] -> Multimap k v
fromList = foldr f empty
  where
    f (k, v) = insert k v

elems :: Multimap k v -> [v]
elems (Multimap m) = Map.elems m >>= Set.elems
