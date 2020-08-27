{-# LANGUAGE OverloadedStrings #-}

module Hecate.Backend.SQLite.Database
  ( put
  , delete
  , query
  , selectAll
  , getCount
  , getCountOfKeyId
  , addKeyId
  , currentSchemaVersion
  , createTable
  , migrate
  ) where

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQLite

import           Hecate.Data            hiding (query)
import           Hecate.Error           (AppError (..))


currentSchemaVersion :: SchemaVersion
currentSchemaVersion = SchemaVersion 2

currentSchema :: SQLite.Query
currentSchema = SQLite.Query t
  where
    t = "CREATE TABLE IF NOT EXISTS entries (\
        \  id          TEXT UNIQUE NOT NULL, \
        \  keyid       TEXT NOT NULL,        \
        \  timestamp   TEXT NOT NULL,        \
        \  description TEXT NOT NULL,        \
        \  identity    TEXT,                 \
        \  ciphertext  TEXT NOT NULL,        \
        \  meta        TEXT                  \
        \)"

createTable :: MonadIO m => SQLite.Connection -> m ()
createTable conn = liftIO (SQLite.execute_ conn currentSchema)

addKeyId :: MonadIO m => SQLite.Connection -> KeyId -> m ()
addKeyId conn keyId = liftIO (SQLite.execute conn s (SQLite.Only keyId))
  where
    s = "INSERT INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta)  \
        \  SELECT id, ?, timestamp, description, identity, ciphertext, meta \
        \  FROM entries_v1"

migrate
  :: (MonadThrow m, MonadIO m)
  => SQLite.Connection
  -> SchemaVersion
  -> KeyId
  -> m ()
migrate conn (SchemaVersion 1) keyId =
  liftIO $ SQLite.withTransaction conn $ do
    SQLite.execute_ conn "ALTER TABLE entries RENAME TO entries_v1"
    createTable conn
    addKeyId conn keyId
    SQLite.execute_ conn "DROP TABLE entries_v1"
migrate _ (SchemaVersion v) _ =
  throwM (Migration ("no supported migration path for schema version " ++ show v))

put :: MonadIO m => SQLite.Connection -> Entry -> m ()
put conn e = liftIO (SQLite.execute conn s e)
  where
    s = "INSERT OR REPLACE INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta) \
        \  VALUES (?, ?, ?, ?, ?, ?, ?)"

delete :: MonadIO m => SQLite.Connection -> Entry -> m ()
delete conn e = liftIO (SQLite.executeNamed conn s [":id" := entryId e])
  where
    s = "DELETE FROM entries WHERE id = :id"

selectAll :: MonadIO m => SQLite.Connection -> m [Entry]
selectAll conn = liftIO (SQLite.query_ conn q)
  where
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries"

processCountResults :: [Count] -> Either AppError Int
processCountResults [count] = Right (unCount count)
processCountResults _       = Left (Database "unexpected results")

getCount :: (MonadThrow m, MonadIO m) => SQLite.Connection -> m Int
getCount conn
  = either throwM pure =<< go
  where
    go = processCountResults <$> liftIO (SQLite.query_ conn q)
    q  = "SELECT count(*) FROM entries"

getCountOfKeyId :: (MonadThrow m, MonadIO m) => SQLite.Connection -> KeyId -> m Int
getCountOfKeyId conn keyId
  = either throwM pure =<< go
  where
    go = processCountResults <$> liftIO (SQLite.queryNamed conn q [":keyid" := keyId])
    q  = "SELECT count(*)     \
         \FROM entries        \
         \WHERE keyid = :keyid"

idMatcher          :: Id          -> (SQLite.Query, [SQLite.NamedParam])
descriptionMatcher :: Description -> (SQLite.Query, [SQLite.NamedParam])
identityMatcher    :: Identity    -> (SQLite.Query, [SQLite.NamedParam])
metadataMatcher    :: Metadata    -> (SQLite.Query, [SQLite.NamedParam])
idMatcher          i               = ("id = :id",                      [":id"          := unId i])
descriptionMatcher (Description d) = ("description LIKE :description", [":description" := "%" <> d <> "%"])
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
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries \
        \WHERE "
    select = (q, [])

query :: MonadIO m => SQLite.Connection -> Query -> m [Entry]
query conn q =
  if queryIsEmpty q
  then selectAll conn
  else liftIO (SQLite.queryNamed conn qs nps)
  where
    (qs, nps) = generateQuery q
