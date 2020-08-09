module Hecate.AppState
  ( EntriesMap
  , AppState
  , appStateDirty
  , appStateData
  , mkAppState
  , mark
  , put
  , delete
  , query
  , selectAll
  , getCount
  , getCountOfKeyId
  ) where

import qualified Data.Map.Strict        as Map
import           Lens.Family2
import           Lens.Family2.Stock     (at)
import           Lens.Family2.Unchecked (lens)

import           Hecate.Data            hiding (query)

-- http://docs.ganeti.org/ganeti/2.12/api/hs/Ganeti/Lens.html
-- http://docs.ganeti.org/ganeti/2.12/api/hs/Ganeti/Utils/MultiMap.html
type EntriesMap = Map.Map Description Entry

data AppState = AppState
  { _appStateDirty :: Bool
  , _appStateData  :: EntriesMap
  } deriving (Show, Eq)

appStateDirty :: Lens' AppState Bool
appStateData  :: Lens' AppState EntriesMap
appStateDirty = lens _appStateDirty (\ s v -> s{_appStateDirty = v})
appStateData  = lens _appStateData  (\ s v -> s{_appStateData  = v})
{-# INLINE appStateDirty #-}
{-# INLINE appStateData  #-}

mkAppState :: [Entry] -> AppState
mkAppState entries = AppState
  { _appStateDirty = False
  , _appStateData  = Map.fromList (tupler <$> entries)
  }
  where
    tupler entry = (entry ^. entryDescription, entry)

mark :: AppState -> AppState
mark = appStateDirty .~ True

put    :: Entry -> AppState -> AppState
delete :: Entry -> AppState -> AppState
put    entry = mark . (appStateData . at (entry ^. entryDescription) .~ Just entry)
delete entry = mark . (appStateData . at (entry ^. entryDescription) .~ Nothing)

query :: Query -> AppState -> [Entry]
query = undefined

selectAll :: AppState -> [Entry]
selectAll = views appStateData Map.elems

getCount :: AppState -> Int
getCount = views appStateData Map.size

getCountOfKeyId :: KeyId -> AppState -> Int
getCountOfKeyId = undefined
