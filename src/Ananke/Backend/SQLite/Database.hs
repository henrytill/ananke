{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ananke.Backend.SQLite.Database
  ( rethrow,
    put,
    delete,
    runQuery,
    selectAll,
    getCount,
    getCountOf,
    addKeyId,
    createTable,
    migrate,
  )
where

import Ananke.Data
import Ananke.Error (AppError (..))
import Control.Exception qualified as Exception
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Database.SQLite3 qualified as SQLite3

rethrow :: IO a -> IO a
rethrow a = Exception.catch a f
  where
    f :: SQLite3.SQLError -> IO a
    f = Exception.throwIO . Database . Text.unpack . SQLite3.sqlErrorDetails

executeStatement :: SQLite3.Statement -> IO ()
executeStatement stmt =
  SQLite3.stepNoCB stmt >>= \case
    SQLite3.Row -> executeStatement stmt
    SQLite3.Done -> return ()

columnMaybeText :: SQLite3.Statement -> SQLite3.ColumnIndex -> (Text -> a) -> IO (Maybe a)
columnMaybeText stmt column f =
  SQLite3.column stmt column >>= \case
    SQLite3.SQLText text -> return . Just . f $ text
    SQLite3.SQLNull -> return Nothing
    _ -> Exception.throwIO . Database $ "unexpected type"

getEntry :: SQLite3.Statement -> IO Entry
getEntry stmt =
  MkEntry
    <$> getTimestamp stmt
    <*> getId stmt
    <*> getKeyId stmt
    <*> getDescription stmt
    <*> getIdentity stmt
    <*> getCiphertext stmt
    <*> getMeta stmt
  where
    getId :: SQLite3.Statement -> IO Id
    getId s = MkId <$> SQLite3.columnText s 0

    getKeyId :: SQLite3.Statement -> IO KeyId
    getKeyId s = MkKeyId <$> SQLite3.columnText s 1

    getDescription :: SQLite3.Statement -> IO Description
    getDescription s = MkDescription <$> SQLite3.columnText s 3

    getTimestamp :: SQLite3.Statement -> IO UTCTime
    getTimestamp s = SQLite3.columnText s 2 >>= utcTimeFromText

    getCiphertext :: SQLite3.Statement -> IO Ciphertext
    getCiphertext s = SQLite3.columnText s 5 >>= ciphertextFromText

    getIdentity :: SQLite3.Statement -> IO (Maybe Identity)
    getIdentity s = columnMaybeText s 4 MkIdentity

    getMeta :: SQLite3.Statement -> IO (Maybe Metadata)
    getMeta s = columnMaybeText s 6 MkMetadata

executeQuery :: SQLite3.Statement -> IO [Entry]
executeQuery = go []
  where
    go acc stmt =
      SQLite3.stepNoCB stmt >>= \case
        SQLite3.Done -> return acc
        SQLite3.Row -> do
          entry <- getEntry stmt
          go (entry : acc) stmt

executeCount :: SQLite3.Statement -> IO Int
executeCount = go 0
  where
    go acc stmt =
      SQLite3.stepNoCB stmt >>= \case
        SQLite3.Done -> return . fromIntegral $ acc
        SQLite3.Row -> do
          count <- SQLite3.columnInt64 stmt 0
          go (count + acc) stmt

createTable :: SQLite3.Database -> IO ()
createTable db = Exception.bracket (SQLite3.prepare db s) SQLite3.finalize executeStatement
  where
    s =
      "CREATE TABLE IF NOT EXISTS entries (\
      \  id TEXT UNIQUE NOT NULL,          \
      \  keyid TEXT NOT NULL,              \
      \  timestamp TEXT NOT NULL,          \
      \  description TEXT NOT NULL,        \
      \  identity TEXT,                    \
      \  ciphertext TEXT NOT NULL,         \
      \  meta TEXT                         \
      \)"

addKeyId :: SQLite3.Database -> KeyId -> IO ()
addKeyId db (MkKeyId keyId) =
  Exception.bracket
    ( do
        stmt <- SQLite3.prepare db s
        SQLite3.bindSQLData stmt 1 (SQLite3.SQLText keyId)
        return stmt
    )
    SQLite3.finalize
    executeStatement
  where
    s =
      "INSERT INTO entries \
      \  (id, keyid, timestamp, description, identity, ciphertext, meta)  \
      \  SELECT id, ?1, timestamp, description, identity, ciphertext, meta \
      \  FROM entries_v1"

migrate ::
  SQLite3.Database ->
  SchemaVersion ->
  KeyId ->
  IO ()
migrate db (MkSchemaVersion 1) keyId = do
  SQLite3.exec db "ALTER TABLE entries RENAME TO entries_v1"
  createTable db
  addKeyId db keyId
  SQLite3.exec db "DROP TABLE entries_v1"
migrate _ (MkSchemaVersion 2) _ =
  return ()
migrate _ (MkSchemaVersion v) _ =
  Exception.throwIO . Migration $ "no supported migration path for schema version " ++ show v

put :: SQLite3.Database -> Entry -> IO ()
put db entry =
  Exception.bracket
    ( do
        stmt <- SQLite3.prepare db s
        SQLite3.bindSQLData stmt 1 . SQLite3.SQLText . unId . entryId $ entry
        SQLite3.bindSQLData stmt 2 . SQLite3.SQLText . unKeyId . entryKeyId $ entry
        SQLite3.bindSQLData stmt 3 . SQLite3.SQLText . utcTimeToText . entryTimestamp $ entry
        SQLite3.bindSQLData stmt 4 . SQLite3.SQLText . unDescription . entryDescription $ entry
        SQLite3.bindSQLData stmt 6 . SQLite3.SQLText . ciphertextToText . entryCiphertext $ entry
        let identity = maybe SQLite3.SQLNull (SQLite3.SQLText . unIdentity) (entryIdentity entry)
            metadata = maybe SQLite3.SQLNull (SQLite3.SQLText . unMetadata) (entryMeta entry)
        SQLite3.bindSQLData stmt 5 identity
        SQLite3.bindSQLData stmt 7 metadata
        return stmt
    )
    SQLite3.finalize
    executeStatement
  where
    s =
      "INSERT OR REPLACE INTO entries \
      \  (id, keyid, timestamp, description, identity, ciphertext, meta) \
      \  VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)"

delete :: SQLite3.Database -> Entry -> IO ()
delete db entry =
  Exception.bracket
    ( do
        stmt <- SQLite3.prepare db s
        SQLite3.bindNamed stmt [(":id", SQLite3.SQLText . unId . entryId $ entry)]
        return stmt
    )
    SQLite3.finalize
    executeStatement
  where
    s = "DELETE FROM entries WHERE id = :id"

selectAll :: SQLite3.Database -> IO [Entry]
selectAll db = Exception.bracket (SQLite3.prepare db s) SQLite3.finalize executeQuery
  where
    s = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta FROM entries"

getCount :: SQLite3.Database -> IO Int
getCount db = Exception.bracket (SQLite3.prepare db s) SQLite3.finalize executeCount
  where
    s = "SELECT count(*) FROM entries"

getCountOf :: SQLite3.Database -> KeyId -> IO Int
getCountOf db (MkKeyId keyId) =
  Exception.bracket
    ( do
        stmt <- SQLite3.prepare db s
        SQLite3.bindNamed stmt [(":keyid", SQLite3.SQLText keyId)]
        return stmt
    )
    SQLite3.finalize
    executeCount
  where
    s = "SELECT count(*) FROM entries WHERE keyid = :keyid"

idMatcher :: Id -> (Text, [(Text, SQLite3.SQLData)])
idMatcher (MkId i) =
  ("id = :id", [(":id", SQLite3.SQLText i)])

descriptionMatcher :: Description -> (Text, [(Text, SQLite3.SQLData)])
descriptionMatcher (MkDescription d) =
  ("description LIKE :description", [(":description", SQLite3.SQLText ("%" <> d <> "%"))])

identityMatcher :: Identity -> (Text, [(Text, SQLite3.SQLData)])
identityMatcher (MkIdentity i) =
  ("identity LIKE :identity", [(":identity", SQLite3.SQLText i)])

metadataMatcher :: Metadata -> (Text, [(Text, SQLite3.SQLData)])
metadataMatcher (MkMetadata m) =
  ("meta LIKE :meta", [(":meta", SQLite3.SQLText m)])

queryFolder ::
  (Text, [(Text, SQLite3.SQLData)]) ->
  Maybe (Text, [(Text, SQLite3.SQLData)]) ->
  (Text, [(Text, SQLite3.SQLData)])
queryFolder ("", []) (Just (qs, np)) = (qs, np)
queryFolder (accQs, accNp) (Just (qs, np)) = (accQs <> " AND " <> qs, accNp <> np)
queryFolder (accQs, accNp) Nothing = (accQs, accNp)

queryParts :: Query -> [Maybe (Text, [(Text, SQLite3.SQLData)])]
queryParts query =
  [ idMatcher <$> queryId query,
    descriptionMatcher <$> queryDescription query,
    identityMatcher <$> queryIdentity query,
    metadataMatcher <$> queryMeta query
  ]

generateQuery :: Query -> (Text, [(Text, SQLite3.SQLData)])
generateQuery = (select <>) . foldl queryFolder ("", []) . queryParts
  where
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta FROM entries WHERE "
    select = (q, [])

runQuery :: SQLite3.Database -> Query -> IO [Entry]
runQuery db query =
  if queryIsEmpty query
    then selectAll db
    else
      Exception.bracket
        ( do
            stmt <- SQLite3.prepare db qs
            SQLite3.bindNamed stmt nps
            return stmt
        )
        SQLite3.finalize
        executeQuery
  where
    (qs, nps) = generateQuery query
