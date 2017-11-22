module Hecate.Interfaces
  ( MonadInteraction(..)
  , MonadStore(..)
  ) where

import           Data.Time.Clock              (UTCTime)

import           Hecate.Data                  (Entry, Query)
import           Hecate.GPG                   (KeyId)

-- * MonadStore

class Monad m => MonadStore m where
  put             :: Entry -> m ()
  delete          :: Entry -> m ()
  query           :: Query -> m [Entry]
  selectAll       :: m [Entry]
  getCount        :: m Int
  getCountOfKeyId :: KeyId -> m Int
  reencryptAll    :: KeyId -> m ()

-- * MonadInteraction

class Monad m => MonadInteraction m where
  now          :: m UTCTime
  prompt       :: String -> m String
  binaryChoice :: String -> m a -> m a -> m a
