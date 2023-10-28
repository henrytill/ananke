module Ananke.Backend.SQLite.AppContext
  ( AppContext (..),
  )
where

import Ananke.Data (Config)
import qualified Database.SQLite3 as SQLite3

-- | 'AppContext' represents the shared environment for computations which occur
-- within our application.  Values of this type are created by 'createContext'.
data AppContext = MkAppContext
  { appContextConfig :: Config,
    appContextDatabase :: SQLite3.Database
  }
