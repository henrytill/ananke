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

import qualified Control.Exception      as Exception
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import qualified Database.SQLite3       as SQLite3

import           Hecate.Data            hiding (query)
import           Hecate.Error           (AppError (..))


catchLift :: (MonadThrow m, MonadIO m) => IO a -> m a
catchLift a = liftIO . Exception.catch a $ \err ->
  do let details = SQLite3.sqlErrorDetails err
     throwM . Database . T.unpack $ details

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = SchemaVersion 2

executeStatement :: SQLite3.Statement -> IO ()
executeStatement stmt = SQLite3.stepNoCB stmt >>= \result ->
  case result of
    SQLite3.Row  -> executeStatement stmt
    SQLite3.Done -> return ()

columnMaybeText :: SQLite3.Statement -> SQLite3.ColumnIndex -> (T.Text -> a) -> IO (Maybe a)
columnMaybeText stmt column f = SQLite3.column stmt column >>= \res ->
  case res of
    SQLite3.SQLText text -> return . Just . f $ text
    SQLite3.SQLNull      -> return Nothing
    _                    -> Exception.throwIO  . Database $ "unexpected type"

getEntry :: SQLite3.Statement -> IO Entry
getEntry stmt
  = Entry <$> getId          stmt
          <*> getKeyId       stmt
          <*> getTimestamp   stmt
          <*> getDescription stmt
          <*> getIdentity    stmt
          <*> getCiphertext  stmt
          <*> getMeta        stmt
  where
    getId          :: SQLite3.Statement -> IO Id
    getKeyId       :: SQLite3.Statement -> IO KeyId
    getDescription :: SQLite3.Statement -> IO Description
    getTimestamp   :: SQLite3.Statement -> IO UTCTime
    getCiphertext  :: SQLite3.Statement -> IO Ciphertext
    getIdentity    :: SQLite3.Statement -> IO (Maybe Identity)
    getMeta        :: SQLite3.Statement -> IO (Maybe Metadata)
    getId          s = Id              <$> SQLite3.columnText s 0
    getKeyId       s = KeyId           <$> SQLite3.columnText s 1
    getDescription s = Description     <$> SQLite3.columnText s 3
    getTimestamp   s = SQLite3.columnText s 2 >>= utcTimeFromText
    getCiphertext  s = SQLite3.columnText s 5 >>= ciphertextFromText
    getIdentity    s = columnMaybeText s 4 Identity
    getMeta        s = columnMaybeText s 6 Metadata

execute :: SQLite3.Statement -> IO [Entry]
execute = go []
  where
    go acc stmt = SQLite3.stepNoCB stmt >>= \result ->
      case result of
        SQLite3.Done -> return acc
        SQLite3.Row  -> do { entry <- getEntry stmt
                           ; go (entry : acc) stmt
                           }

executeCount :: SQLite3.Statement -> IO Int
executeCount = go 0
  where
    go acc stmt = SQLite3.stepNoCB stmt >>= \result ->
      case result of
        SQLite3.Done -> return . fromIntegral $ acc
        SQLite3.Row  -> do { count <- SQLite3.columnInt64 stmt 0
                           ; go (acc + count) stmt
                           }

createTable :: (MonadThrow m, MonadIO m) => SQLite3.Database -> m ()
createTable db = catchLift $ Exception.bracket
  (SQLite3.prepare db s)
  SQLite3.finalize
  executeStatement
  where
    s  = "CREATE TABLE IF NOT EXISTS entries (\
         \  id          TEXT UNIQUE NOT NULL, \
         \  keyid       TEXT NOT NULL,        \
         \  timestamp   TEXT NOT NULL,        \
         \  description TEXT NOT NULL,        \
         \  identity    TEXT,                 \
         \  ciphertext  TEXT NOT NULL,        \
         \  meta        TEXT                  \
         \)"

addKeyId :: (MonadThrow m, MonadIO m) => SQLite3.Database -> KeyId -> m ()
addKeyId db (KeyId keyId) = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; SQLite3.bindSQLData stmt 1 (SQLite3.SQLText keyId)
      ; return stmt
      })
  SQLite3.finalize
  executeStatement
  where
    s = "INSERT INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta)  \
        \  SELECT id, ?1, timestamp, description, identity, ciphertext, meta \
        \  FROM entries_v1"

migrate
  :: (MonadThrow m, MonadIO m)
  => SQLite3.Database
  -> SchemaVersion
  -> KeyId
  -> m ()
migrate db (SchemaVersion 1) keyId =
  catchLift $ do
    SQLite3.exec db "ALTER TABLE entries RENAME TO entries_v1"
    createTable db
    addKeyId db keyId
    SQLite3.exec db "DROP TABLE entries_v1"
migrate _ (SchemaVersion v) _ =
  throwM . Migration $ "no supported migration path for schema version " ++ show v

put :: (MonadThrow m, MonadIO m) => SQLite3.Database -> Entry -> m ()
put db e = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; SQLite3.bindSQLData stmt 1 (SQLite3.SQLText . unId             . entryId          $ e)
      ; SQLite3.bindSQLData stmt 2 (SQLite3.SQLText . unKeyId          . entryKeyId       $ e)
      ; SQLite3.bindSQLData stmt 3 (SQLite3.SQLText . utcTimeToText    . entryTimestamp   $ e)
      ; SQLite3.bindSQLData stmt 4 (SQLite3.SQLText . unDescription    . entryDescription $ e)
      ; SQLite3.bindSQLData stmt 6 (SQLite3.SQLText . ciphertextToText . entryCiphertext  $ e)
      ; let { identity = fromMaybe SQLite3.SQLNull (SQLite3.SQLText . unIdentity <$> entryIdentity e)
            ; metadata = fromMaybe SQLite3.SQLNull (SQLite3.SQLText . unMetadata <$> entryMeta     e)
            }
      ; SQLite3.bindSQLData stmt 5 identity
      ; SQLite3.bindSQLData stmt 7 metadata
      ; return stmt
      })
  SQLite3.finalize
  executeStatement
  where
    s = "INSERT OR REPLACE INTO entries \
        \  (id, keyid, timestamp, description, identity, ciphertext, meta) \
        \  VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)"

delete :: (MonadThrow m, MonadIO m) => SQLite3.Database -> Entry -> m ()
delete db e = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; SQLite3.bindNamed stmt [(":id", SQLite3.SQLText . unId . entryId $ e)]
      ; return stmt
      })
  SQLite3.finalize
  executeStatement
  where
    s = "DELETE FROM entries WHERE id = :id"

selectAll :: (MonadThrow m, MonadIO m) => SQLite3.Database -> m [Entry]
selectAll db = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; return stmt
      })
  SQLite3.finalize
  execute
  where
    s = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries"

getCount :: (MonadThrow m, MonadIO m) => SQLite3.Database -> m Int
getCount db = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; return stmt
      })
  SQLite3.finalize
  executeCount
  where
    s = "SELECT count(*) FROM entries"

getCountOfKeyId :: (MonadThrow m, MonadIO m) => SQLite3.Database -> KeyId -> m Int
getCountOfKeyId db (KeyId keyId) = catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db s
      ; SQLite3.bindNamed stmt [(":keyid", SQLite3.SQLText keyId)]
      ; return stmt
      })
  SQLite3.finalize
  executeCount
  where
    s = "SELECT count(*)     \
        \FROM entries        \
        \WHERE keyid = :keyid"

idMatcher          :: Id          -> (T.Text, [(T.Text, SQLite3.SQLData)])
descriptionMatcher :: Description -> (T.Text, [(T.Text, SQLite3.SQLData)])
identityMatcher    :: Identity    -> (T.Text, [(T.Text, SQLite3.SQLData)])
metadataMatcher    :: Metadata    -> (T.Text, [(T.Text, SQLite3.SQLData)])
idMatcher          (Id i)          = ("id = :id",                      [(":id",          SQLite3.SQLText i)])
descriptionMatcher (Description d) = ("description LIKE :description", [(":description", SQLite3.SQLText ("%" <> d <> "%"))])
identityMatcher    (Identity i)    = ("identity LIKE :identity",       [(":identity",    SQLite3.SQLText i)])
metadataMatcher    (Metadata m)    = ("meta LIKE :meta",               [(":meta",        SQLite3.SQLText m)])

queryFolder
  :: (T.Text, [(T.Text, SQLite3.SQLData)])
  -> Maybe (T.Text, [(T.Text, SQLite3.SQLData)])
  -> (T.Text, [(T.Text, SQLite3.SQLData)])
queryFolder ("", [])       (Just (qs, np)) = (qs, np)
queryFolder (accQs, accNp) (Just (qs, np)) = (accQs <> " AND " <> qs, accNp <> np)
queryFolder (accQs, accNp) Nothing         = (accQs, accNp)

queryParts :: Query -> [Maybe (T.Text, [(T.Text, SQLite3.SQLData)])]
queryParts q =
  [ idMatcher          <$> queryId q
  , descriptionMatcher <$> queryDescription q
  , identityMatcher    <$> queryIdentity q
  , metadataMatcher    <$> queryMeta q
  ]

generateQuery :: Query -> (T.Text, [(T.Text, SQLite3.SQLData)])
generateQuery = (select <>) . foldl queryFolder ("", []) . queryParts
  where
    q = "SELECT id, keyid, timestamp, description, identity, ciphertext, meta \
        \FROM entries \
        \WHERE "
    select = (q, [])

query :: (MonadThrow m, MonadIO m) => SQLite3.Database -> Query -> m [Entry]
query db q =
  if queryIsEmpty q
  then selectAll db
  else catchLift $ Exception.bracket
  (do { stmt <- SQLite3.prepare db qs
      ; SQLite3.bindNamed stmt nps
      ; return stmt
      })
  SQLite3.finalize
  execute
  where
    (qs, nps) = generateQuery q
