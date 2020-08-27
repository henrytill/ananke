module Hecate.Backend.SQLite.AppContext
  ( AppContext (..)
  ) where

import qualified Database.SQLite.Simple as SQLite

import           Hecate.Data            (Config)


-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = AppContext
  { appContextConfig     :: Config
  , appContextConnection :: SQLite.Connection
  }
