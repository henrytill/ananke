{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hecate.Server (app) where

import Control.Monad.Except
import Hecate.IO
import Hecate.Types
import Hecate.Util
import Servant

type API = "entries" :> Get '[JSON] [Entry]

server :: Server API
server = enter convert es
  where
    entries :: (MonadIO m, MonadError Error m) => m [Entry]
    entries = do
      mk <- loadAuth (MasterPassword "zardoz") "/Users/ht/.hecate/auth.json"
      e1 <- entry mk (Description "first entry") (Just (Identity "ht@xngns.net")) (PlainText "notarealpassword") Nothing
      return [e1]

    es :: App [Entry]
    es = App entries

    errorToServantError :: Error -> ServantErr
    errorToServantError = const err500

    convertError :: Monad m => ExceptT Error m a -> ExceptT ServantErr m a
    convertError = bimapExceptT errorToServantError id

    convert :: App :~> ExceptT ServantErr IO
    convert = Nat $ convertError . unApp

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
