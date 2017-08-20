{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Hecate.Database
  ( createContext
  , put
  , delete
  , query
  , selectAll
  , checkEntries
  ) where

import           Control.Monad.Except
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple (NamedParam ((:=)))
import           System.Directory       (doesFileExist)

import           Hecate.Context
import           Hecate.Data            hiding (query)
import           Hecate.Error
import           Hecate.GPG (KeyId)


-- | A 'SchemaVersion' represents the database's schema version
newtype SchemaVersion = SchemaVersion { unSchemaVersion :: Int }
  deriving Eq

instance Show SchemaVersion where
  show = show . unSchemaVersion

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = SchemaVersion 2

currentSchema :: T.Text -> SQLite.Query
currentSchema name = SQLite.Query t
  where
    t = "CREATE TABLE IF NOT EXISTS " `T.append` name `T.append` " (\
        \  id          TEXT UNIQUE NOT NULL,                        \
        \  keyid       TEXT NOT NULL,                               \
        \  timestamp   TEXT NOT NULL,                               \
        \  description TEXT NOT NULL,                               \
        \  identity    TEXT,                                        \
        \  ciphertext  TEXT NOT NULL,                               \
        \  meta        TEXT                                         \
        \)"

createSchemaFile :: MonadIO m => FilePath -> m ()
createSchemaFile path = liftIO (writeFile path (show currentSchemaVersion))

getSchemaVersionFromFile :: MonadIO m => FilePath -> m SchemaVersion
getSchemaVersionFromFile path = (SchemaVersion . read) <$> liftIO (readFile path)

getSchemaVersion :: MonadIO m => FilePath -> m SchemaVersion
getSchemaVersion path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then getSchemaVersionFromFile path
    else createSchemaFile path >> pure currentSchemaVersion

addKeyId :: SQLite.Connection -> KeyId -> IO ()
addKeyId conn keyId = SQLite.execute conn s (SQLite.Only keyId)
  where
    s = "INSERT INTO new_entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta)  \
        \  SELECT id, ?, timestamp, description, identity, ciphertext, meta \
        \  FROM entries"

migrate
  :: (MonadIO m, MonadError AppError m)
  => SQLite.Connection
  -> FilePath
  -> SchemaVersion
  -> KeyId
  -> m ()
migrate conn path (SchemaVersion 1) keyId = do
  liftIO $ SQLite.withTransaction conn $ do
    SQLite.execute_ conn (currentSchema "new_entries")
    addKeyId conn keyId
    SQLite.execute_ conn "DROP TABLE entries"
    SQLite.execute_ conn "ALTER TABLE new_entries RENAME TO entries"
  reencryptAll conn keyId
  createSchemaFile path
migrate _ _ (SchemaVersion v) _ =
  throwError $ MigrationError ("no supported migration path for schema version " ++ show v)

initDatabase
  :: (MonadIO m, MonadError AppError m)
  => SQLite.Connection
  -> FilePath
  -> SchemaVersion
  -> KeyId
  -> m ()
initDatabase conn path schemaVersion keyId =
  if schemaVersion == currentSchemaVersion
  then liftIO $ SQLite.execute_ conn (currentSchema "entries")
  else do
    liftIO $ putStrLn ("Migrating database from schema version "
                       ++ show schemaVersion
                       ++ " to version "
                       ++ show currentSchemaVersion
                       ++ "...")
    migrate conn path schemaVersion keyId

createContext :: (MonadIO m, MonadError AppError m) => AppConfig -> m AppContext
createContext config = do
  connection    <- liftIO $ SQLite.open dbFile
  schemaVersion <- getSchemaVersion schemaFile
  initDatabase connection schemaFile schemaVersion keyId
  return $ AppContext keyId connection
  where
    schemaFile = appConfigDataDirectory config ++ "/db/schema"
    dbFile     = appConfigDataDirectory config ++ "/db/db.sqlite"
    keyId      = appConfigKeyId config

put :: MonadIO m => SQLite.Connection -> Entry -> m ()
put conn e = liftIO $ SQLite.execute conn s e
  where
    s = "INSERT OR REPLACE INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta) \
        \  VALUES (?, ?, ?, ?, ?, ?, ?)"

delete :: MonadIO m => SQLite.Connection -> Entry -> m ()
delete conn e = liftIO $ SQLite.executeNamed conn s [":id" := entryId e]
  where
    s = "DELETE FROM entries WHERE id = :id"

selectAll :: MonadIO m => SQLite.Connection -> m [Entry]
selectAll conn = liftIO $ SQLite.query_ conn q
  where
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries"

checkEntries :: (MonadIO m, MonadError AppError m) => SQLite.Connection -> KeyId -> m Bool
checkEntries conn keyId = liftIO $ SQLite.queryNamed conn q r >>= p
  where
    q = "SELECT count(keyid)  \
        \FROM entries         \
        \WHERE keyid != :keyid"
    r = [":keyid" := keyId]
    p [count] = return (unCount count == 0)
    p _       = return False

reencryptAll :: (MonadIO m, MonadError AppError m) => SQLite.Connection -> KeyId -> m ()
reencryptAll conn keyId = do
  es  <- selectAll conn
  ues <- mapM (updateKeyId keyId) es
  mapM_ (put conn) ues
  mapM_ (delete conn) es

idMatcher          :: Id          -> (SQLite.Query, [SQLite.NamedParam])
descriptionMatcher :: Description -> (SQLite.Query, [SQLite.NamedParam])
identityMatcher    :: Identity    -> (SQLite.Query, [SQLite.NamedParam])
metadataMatcher    :: Metadata    -> (SQLite.Query, [SQLite.NamedParam])
idMatcher          i               = ("id = :id",                      [":id"          := unId i])
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
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries \
        \WHERE "
    select = (q, [])

query :: MonadIO m => SQLite.Connection -> Query -> m [Entry]
query conn q =
  if queryIsEmpty q
  then selectAll conn
  else liftIO $ SQLite.queryNamed conn qs nps
  where
    (qs, nps) = generateQuery q
