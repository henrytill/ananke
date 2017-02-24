{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hecate.Database where

import Control.Monad.Except
import Database.SQLite.Simple hiding (Error)
import Hecate.Types

initDatabase :: MonadIO m => Connection -> m ()
initDatabase conn = liftIO $ execute_ conn s
  where
    s = "CREATE TABLE IF NOT EXISTS entries (\
        \        nonce       BLOB NOT NULL,  \
        \        authTag     BLOB NOT NULL,  \
        \        timeStamp   TEXT NOT NULL,  \
        \        description TEXT NOT NULL,  \
        \        identity    TEXT,           \
        \        cipherText  BLOB NOT NULL,  \
        \        meta        TEXT            \
        \)"

insert :: MonadIO m => Connection -> Entry -> m ()
insert conn e = liftIO $ execute conn s e
  where
    s = "INSERT INTO entries \
        \  (nonce, authTag, timeStamp, description, identity, cipherText, meta) \
        \  VALUES (?, ?, ?, ?, ?, ?, ?)"

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

getOne :: (MonadIO m, MonadError Error m) => Connection -> Nonce -> m Entry
getOne conn nce = do
  let q = "SELECT * FROM entries WHERE nonce = :nonce"
  res <- liftIO $ queryNamed conn q [":nonce" := nce]
  case head' res of
    Just x  -> pure x
    Nothing -> throwError (NotFound nce)

getAll :: MonadIO m => Connection -> m [Entry]
getAll conn = liftIO $ query_ conn q
  where
    q = "SELECT * FROM entries"
