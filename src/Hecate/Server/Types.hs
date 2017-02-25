{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Server.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Hecate.Types
import qualified Database.SQLite.Simple as SQLite

-- | 'ServerError' represents errors that occur on the server
data ServerError
  = Default String
  | NotFound Nonce
  deriving (Show, Eq)

newtype ServerContext = ServerContext { _conn :: SQLite.Connection }

type ServerStack a = ReaderT ServerContext (ExceptT ServerError IO) a

newtype ServerApp a = ServerApp { unServerApp :: ServerStack a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ServerError
           , MonadIO
           , MonadReader ServerContext
           )

runServerApp
  :: ServerContext
  -> ServerApp a
  -> IO (Either ServerError a)
runServerApp ctx = runExceptT . flip runReaderT ctx . unServerApp
