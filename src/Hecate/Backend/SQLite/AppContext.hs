module Hecate.Backend.SQLite.AppContext
  ( AppContext (..)
  , HasAppContext (..)
  ) where

import qualified Database.SQLite.Simple as SQLite
import           Lens.Family2           (Lens')
import           Lens.Family2.Unchecked (lens)

import           Hecate.Data            (Config, HasConfig (..))


-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { _appContextConfig     :: Config
  , _appContextConnection :: SQLite.Connection
  }

instance HasConfig AppContext where
  config = lens _appContextConfig (\ a v -> a{_appContextConfig = v})
  {-# INLINE config #-}

class HasConfig t => HasAppContext t where
  appContext           :: Lens' t AppContext
  appContextConfig     :: Lens' t Config
  appContextConnection :: Lens' t SQLite.Connection
  appContextConfig     = appContext . appContextConfig
  appContextConnection = appContext . appContextConnection
  {-# INLINE appContextConfig     #-}
  {-# INLINE appContextConnection #-}

instance HasAppContext AppContext where
  appContext           = id
  appContextConfig     = config
  appContextConnection = lens _appContextConnection (\ a v -> a{_appContextConnection = v})
  {-# INLINE appContext           #-}
  {-# INLINE appContextConfig     #-}
  {-# INLINE appContextConnection #-}
