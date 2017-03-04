{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hecate.Database where

import Control.Monad.Except
import Data.Monoid
import Hecate.Types
import Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQLite

initDatabase :: MonadIO m => SQLite.Connection -> m ()
initDatabase conn = liftIO $ SQLite.execute_ conn s
  where
    s = "CREATE TABLE IF NOT EXISTS entries (\
        \  id          TEXT UNIQUE NOT NULL, \
        \  timestamp   TEXT NOT NULL,        \
        \  description TEXT NOT NULL,        \
        \  identity    TEXT,                 \
        \  ciphertext  BLOB NOT NULL,        \
        \  meta        TEXT                  \
        \)"

put :: MonadIO m => SQLite.Connection -> Entry -> m ()
put conn e = liftIO $ SQLite.execute conn s e
  where
    s = "INSERT OR REPLACE INTO entries \
        \  (id, timestamp, description, identity, ciphertext, meta) \
        \  VALUES (?, ?, ?, ?, ?, ?)"

delete :: MonadIO m => SQLite.Connection -> Entry -> m ()
delete conn Entry{..} = liftIO $ SQLite.executeNamed conn s [":id" := entryId]
  where
    s = "DELETE FROM entries WHERE id = :id"

selectAll :: MonadIO m => SQLite.Connection -> m [Entry]
selectAll conn = liftIO $ SQLite.query_ conn q
  where
    q = "SELECT * FROM entries"

idMatcher          :: Id          -> (SQLite.Query, [SQLite.NamedParam])
descriptionMatcher :: Description -> (SQLite.Query, [SQLite.NamedParam])
identityMatcher    :: Identity    -> (SQLite.Query, [SQLite.NamedParam])
metadataMatcher    :: Metadata    -> (SQLite.Query, [SQLite.NamedParam])
idMatcher          (Id n)          = ("id = :id",                      [":id"          := n])
descriptionMatcher (Description d) = ("description LIKE :description", [":description" := d])
identityMatcher    (Identity i)    = ("identity LIKE :identity",       [":identity"    := i])
metadataMatcher    (Metadata m)    = ("meta LIKE :meta",               [":meta"        := m])

queryFolder
  :: (SQLite.Query, [SQLite.NamedParam])
  -> Maybe (SQLite.Query, [SQLite.NamedParam])
  -> (SQLite.Query, [SQLite.NamedParam])
queryFolder ("", [])       (Just (qs, np)) = (qs, np)
queryFolder (accQs, accNp) (Just (qs, np)) = (accQs <> " AND " <> qs, accNp <> np)
queryFolder (accQs, accNp) Nothing         = (accQs, accNp)

queryParts :: Query -> [Maybe (SQLite.Query, [SQLite.NamedParam])]
queryParts q =
  [ idMatcher          <$> queryId q
  , descriptionMatcher <$> queryDescription q
  , identityMatcher    <$> queryIdentity q
  , metadataMatcher    <$> queryMeta q
  ]

generateQuery :: Query -> (SQLite.Query, [SQLite.NamedParam])
generateQuery = (select <>) . foldl queryFolder ("", []) . queryParts
  where
    select = ("SELECT * FROM entries WHERE ", [])

query :: MonadIO m => SQLite.Connection -> Query -> m [Entry]
query conn (Query Nothing Nothing Nothing Nothing) = selectAll conn
query conn q                                       = liftIO $ SQLite.queryNamed conn qs nps
  where
    (qs, nps) = generateQuery q
