{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Server
  ( app
  , api
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Hecate.Server.Database
import Hecate.Server.Types
import Hecate.Types
import Hecate.Util
import Servant

type API = "api" :>
           (    ReqBody '[JSON] Entry :> Put    '[JSON] Ok
           :<|> ReqBody '[JSON] Entry :> Delete '[JSON] Ok
           :<|> ReqBody '[JSON] Query :> Get    '[JSON] [Entry]
           )

endpoints :: ServerT API ServerApp
endpoints = putEntry :<|> deleteEntry :<|> queryEntries

putEntry
  :: (MonadIO m, MonadReader ServerContext m)
  => Entry
  -> m Ok
putEntry e = do
  ctx <- ask
  _   <- put (_conn ctx) e
  return (Ok "put")

deleteEntry
  :: (MonadIO m, MonadReader ServerContext m)
  => Entry
  -> m Ok
deleteEntry e = do
  ctx <- ask
  _   <- delete (_conn ctx) e
  return (Ok "delete")

queryEntries
  :: (MonadIO m, MonadReader ServerContext m)
  => Query
  -> m [Entry]
queryEntries q = do
  ctx <- ask
  query (_conn ctx) q

errorToServantError :: ServerError -> ServantErr
errorToServantError = const err500

convertError :: Monad m => ExceptT ServerError m a -> ExceptT ServantErr m a
convertError = bimapExceptT errorToServantError id

convert :: ServerContext -> ServerApp :~> ExceptT ServantErr IO
convert ctx = Nat $ convertError . flip runReaderT ctx . unServerApp

server :: ServerContext -> Server API
server appContext = enter (convert appContext) endpoints

api :: Proxy API
api = Proxy

app :: ServerContext -> Application
app = serve api . server
