{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Server (app) where

import Control.Monad.Except
import Control.Monad.Reader
import Hecate.Server.Database
import Hecate.Server.Types
import Hecate.Types
import Hecate.Util
import Servant

type API = "entries" :> Get '[JSON] [Entry]

server :: ServerContext -> Server API
server appContext = enter (convert appContext) (ServerApp entries)
  where
    entries
      :: (MonadIO m, MonadReader ServerContext m)
      => m [Entry]
    entries = do
      ctx <- ask
      getAll (_conn ctx)

    errorToServantError :: ServerError -> ServantErr
    errorToServantError = const err500

    convertError :: Monad m => ExceptT ServerError m a -> ExceptT ServantErr m a
    convertError = bimapExceptT errorToServantError id

    convert :: ServerContext -> ServerApp :~> ExceptT ServantErr IO
    convert ctx = Nat $ convertError . flip runReaderT ctx . unServerApp

api :: Proxy API
api = Proxy

app :: ServerContext -> Application
app = serve api . server
