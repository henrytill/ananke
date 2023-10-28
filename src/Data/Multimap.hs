module Data.Multimap
  ( Multimap,
    empty,
    null,
    insert,
    deleteAll,
    delete,
    lookup,
    fromList,
    elems,
    size,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (foldl, foldr, lookup, null)
import qualified Prelude

newtype Multimap k v = MkMultimap {unMultimap :: Map k (Set v)}
  deriving (Eq, Ord, Show)

empty :: Multimap k v
empty = MkMultimap Map.empty

null :: Multimap k v -> Bool
null (MkMultimap m) = Map.null m

inserter :: (Ord v) => v -> Maybe (Set v) -> Maybe (Set v)
inserter v Nothing = Just (Set.singleton v)
inserter v (Just s) = Just (Set.insert v s)

insert :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
insert k v (MkMultimap m) = MkMultimap $ Map.alter (inserter v) k m

deleteAll :: (Ord k) => k -> Multimap k v -> Multimap k v
deleteAll k (MkMultimap m) = MkMultimap $ Map.delete k m

deleter :: (Ord v) => v -> Maybe (Set v) -> Maybe (Set v)
deleter _ Nothing = Nothing
deleter v (Just s)
  | Set.null updated = Nothing
  | otherwise = Just updated
  where
    updated = Set.delete v s

delete :: (Ord k, Ord v) => k -> v -> Multimap k v -> Multimap k v
delete k v (MkMultimap m) = MkMultimap $ Map.alter (deleter v) k m

lookup :: (Ord k) => k -> Multimap k v -> Set v
lookup k (MkMultimap m) = Maybe.fromMaybe Set.empty (Map.lookup k m)

fromList :: (Ord k, Ord v) => [(k, v)] -> Multimap k v
fromList = Prelude.foldr f empty
  where
    f (k, v) = insert k v

elems :: Multimap k v -> [v]
elems (MkMultimap m) = Map.elems m >>= Set.elems

size :: Multimap k v -> Int
size = length . elems

foldr :: (Set a -> b -> b) -> b -> Multimap k a -> b
foldr f b (MkMultimap m) = Map.foldr f b m

foldl :: (a -> Set b -> a) -> a -> Multimap k b -> a
foldl f a (MkMultimap m) = Map.foldl f a m

foldrWithKey :: (k -> Set a -> b -> b) -> b -> Multimap k a -> b
foldrWithKey f b (MkMultimap m) = Map.foldrWithKey f b m

foldlWithKey :: (a -> k -> Set b -> a) -> a -> Multimap k b -> a
foldlWithKey f a (MkMultimap m) = Map.foldlWithKey f a m

foldMapWithKey :: (Monoid m) => (k -> Set a -> m) -> Multimap k a -> m
foldMapWithKey f (MkMultimap m) = Map.foldMapWithKey f m
