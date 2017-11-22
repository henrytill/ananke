{-# LANGUAGE OverloadedStrings #-}

module Hecate.Database
  ( setup
  , put
  , delete
  , query
  , selectAll
  , getCount
  , getCountOfKeyId
  , reencryptAll
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple (NamedParam ((:=)))
import           Lens.Family2
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
  exists <- liftIO (doesFileExist path)
  if exists
    then getSchemaVersionFromFile path
    else createSchemaFile path >> return currentSchemaVersion

addKeyId :: SQLite.Connection -> KeyId -> IO ()
addKeyId conn keyId = SQLite.execute conn s (SQLite.Only keyId)
  where
    s = "INSERT INTO new_entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta)  \
        \  SELECT id, ?, timestamp, description, identity, ciphertext, meta \
        \  FROM entries"

migrate
  :: MonadIO m
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
  liftIO (throwIO (Migration ("no supported migration path for schema version " ++ show v)))

initDatabase
  :: MonadIO m
  => SQLite.Connection
  -> FilePath
  -> SchemaVersion
  -> KeyId
  -> m ()
initDatabase conn path schemaVersion keyId =
  if schemaVersion == currentSchemaVersion
  then liftIO (SQLite.execute_ conn (currentSchema "entries"))
  else do
    liftIO $ putStrLn ("Migrating database from schema version "
                       ++ show schemaVersion
                       ++ " to version "
                       ++ show currentSchemaVersion
                       ++ "...")
    migrate conn path schemaVersion keyId

setup :: (MonadIO m, MonadReader r m, HasAppContext r) => m ()
setup = do
  ctx <- ask
  let schemaFile = ctx ^. configSchemaFile
      keyId      = ctx ^. configKeyId
      connection = ctx ^. appContextConnection
  schemaVersion <- getSchemaVersion schemaFile
  initDatabase connection schemaFile schemaVersion keyId

put :: MonadIO m => SQLite.Connection -> Entry -> m ()
put conn e = liftIO (SQLite.execute conn s e)
  where
    s = "INSERT OR REPLACE INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta) \
        \  VALUES (?, ?, ?, ?, ?, ?, ?)"

delete :: MonadIO m => SQLite.Connection -> Entry -> m ()
delete conn e = liftIO (SQLite.executeNamed conn s [":id" := _entryId e])
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

getCount :: MonadIO m => SQLite.Connection -> m Int
getCount conn
  = either (liftIO . throwIO) pure =<< go
  where
    go = processCountResults <$> liftIO (SQLite.query_ conn q)
    q  = "SELECT count(*) FROM entries"

getCountOfKeyId :: MonadIO m => SQLite.Connection -> KeyId -> m Int
getCountOfKeyId conn keyId
  = either (liftIO . throwIO) pure =<< go
  where
    go = processCountResults <$> liftIO (SQLite.queryNamed conn q [":keyid" := keyId])
    q  = "SELECT count(*)     \
         \FROM entries        \
         \WHERE keyid = :keyid"

reencryptAll :: MonadIO m => SQLite.Connection -> KeyId -> m ()
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
  [ idMatcher          <$> _queryId q
  , descriptionMatcher <$> _queryDescription q
  , identityMatcher    <$> _queryIdentity q
  , metadataMatcher    <$> _queryMeta q
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
