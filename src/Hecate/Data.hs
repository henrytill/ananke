{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Hecate.Data
  ( -- * Import & Display Entries
    ImportEntry
  , DisplayEntry(..)
  , decryptEntry
  , decryptEntries
    -- * Entries
  , Entry
  , entryId
  , createEntry
  , importEntryToEntry
    -- ** Their constituents
  , Id
  , unId
  , Description(..)
  , Identity(..)
  , Metadata(..)
    -- ** And some updaters
  , updateDescription
  , updateIdentity
  , updateCiphertext
  , updateMetadata
    -- * Queries
  , Query
  , queryId
  , queryIdentity
  , queryDescription
  , queryMeta
    -- ** Query helpers
  , queryFromDescription
  , queryIsEmpty
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite

import Hecate.Context
import Hecate.Error
import Hecate.GPG

{-# ANN module "HLint: ignore Use newtype instead of data" #-}


-- * Import and Display Entries

-- | An 'ImportEntry' is a record that is imported or exported from a CSV file
data ImportEntry = ImportEntry
  { importDescription :: Description
  , importIdentity    :: Maybe Identity
  , importPlaintext   :: Plaintext
  , importMeta        :: Maybe Metadata
  } deriving (Generic, Show, Eq)

instance CSV.FromRecord ImportEntry
instance CSV.ToRecord ImportEntry

-- | A 'DisplayEntry' is a record that is displayed to the user in response to a
-- command
data DisplayEntry = DisplayEntry
  { displayTimestamp   :: UTCTime
  , displayDescription :: Description
  , displayIdentity    :: Maybe Identity
  , displayPlaintext   :: Plaintext
  , displayMeta        :: Maybe Metadata
  } deriving (Show, Eq)

entryToDisplayEntry :: Entry -> Plaintext -> DisplayEntry
entryToDisplayEntry Entry{..} p =
  DisplayEntry entryTimestamp entryDescription entryIdentity p entryMeta

getPlainText
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Entry
  -> m Plaintext
getPlainText Entry{..} = decrypt entryCiphertext

decryptEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Entry
  -> m DisplayEntry
decryptEntry e = entryToDisplayEntry e <$> getPlainText e

decryptEntries
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => [Entry]
  -> m [DisplayEntry]
decryptEntries = mapM decryptEntry


-- * Entries

-- | An 'Entry' is a record that stores an encrypted value along with associated
-- information
data Entry = Entry
  { entryId          :: Id
  , entryTimestamp   :: UTCTime
  , entryDescription :: Description
  , entryIdentity    :: Maybe Identity
  , entryCiphertext  :: Ciphertext
  , entryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

instance SQLite.FromRow Entry where
  fromRow = Entry <$> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field
                  <*> SQLite.field

instance SQLite.ToRow Entry where
  toRow Entry{..} =
    SQLite.toRow (entryId, entryTimestamp, entryDescription, entryIdentity, entryCiphertext, entryMeta)

showTime :: UTCTime -> T.Text
showTime = T.pack . formatTime defaultTimeLocale "%s%Q"

ider :: T.Text -> Id
ider = Id . T.pack . showDigest . sha1 . BSL.fromStrict . encodeUtf8

createId :: UTCTime -> Description -> Maybe Identity -> Id
createId ts (Description d) (Just (Identity i)) = ider $ showTime ts <> d <> i
createId ts (Description d) Nothing             = ider $ showTime ts <> d

createEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Description
  -> Maybe Identity
  -> Plaintext
  -> Maybe Metadata
  -> m Entry
createEntry description identity plaintext meta = do
  timestamp <- liftIO getCurrentTime
  i         <- pure $ createId timestamp description identity
  encrypted <- encrypt plaintext
  return $ Entry i timestamp description identity encrypted meta

importEntryToEntry
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => ImportEntry
  -> m Entry
importEntryToEntry ImportEntry{..} =
  createEntry importDescription importIdentity importPlaintext importMeta


-- ** Their constituents

-- | A 'Id' identifies a given 'Entry'.
newtype Id = Id { unId :: T.Text }
  deriving Eq

instance Show Id where
  show (Id d) = show d

instance ToField Id where
  toField (Id bs) = toField bs

instance FromField Id where
  fromField f = Id <$> fromField f

-- | A 'Description' identifies a given 'Entry'.  It could be a URI or a
-- descriptive name.
newtype Description = Description T.Text
  deriving Eq

instance Show Description where
  show (Description d) = show d

instance ToField Description where
  toField (Description bs) = toField bs

instance FromField Description where
  fromField f = Description <$> fromField f

instance CSV.ToField Description where
  toField (Description bs) = CSV.toField bs

instance CSV.FromField Description where
  parseField f = Description <$> CSV.parseField f

-- | An 'Identity' represents an identifying value.  It could be the username in
-- a username/password pair
newtype Identity = Identity T.Text
  deriving Eq

instance Show Identity where
  show (Identity i) = show i

instance ToField Identity where
  toField (Identity bs) = toField bs

instance FromField Identity where
  fromField f = Identity <$> fromField f

instance CSV.ToField Identity where
  toField (Identity bs) = CSV.toField bs

instance CSV.FromField Identity where
  parseField f = Identity <$> CSV.parseField f

-- | A 'Metadata' value contains additional non-specific information for a given
-- 'Entry'
newtype Metadata = Metadata T.Text
  deriving Eq

instance Show Metadata where
  show (Metadata m) = show m

instance ToField Metadata where
  toField (Metadata bs) = toField bs

instance FromField Metadata where
  fromField f = Metadata <$> fromField f

instance CSV.ToField Metadata where
  toField (Metadata bs) = CSV.toField bs

instance CSV.FromField Metadata where
  parseField f = Metadata <$> CSV.parseField f


-- ** And some updaters

withUpdateTimestamp :: MonadIO m => (Entry -> Entry) -> Entry -> m Entry
withUpdateTimestamp f e = f <$> g
  where
    g = (\ts -> e{entryTimestamp = ts}) <$> liftIO getCurrentTime

updateIdentity :: MonadIO m => Maybe Identity -> Entry -> m Entry
updateMetadata :: MonadIO m => Maybe Metadata -> Entry -> m Entry
updateIdentity i = withUpdateTimestamp $ \ue -> ue{entryIdentity = i}
updateMetadata m = withUpdateTimestamp $ \ue -> ue{entryMeta = m}

updateDescription
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Description
  -> Entry
  -> m Entry
updateDescription d e@Entry{entryIdentity, entryMeta} = do
  pt <- getPlainText e
  createEntry d entryIdentity pt entryMeta

updateCiphertext
  :: (MonadIO m, MonadReader AppContext m, MonadError AppError m)
  => Plaintext
  -> Entry
  -> m Entry
updateCiphertext pt Entry{entryDescription, entryIdentity, entryMeta} =
  createEntry entryDescription entryIdentity pt entryMeta


-- * Queries

-- | A 'Query' represents a database query
data Query = Query
  { queryId          :: Maybe Id
  , queryDescription :: Maybe Description
  , queryIdentity    :: Maybe Identity
  , queryMeta        :: Maybe Metadata
  } deriving (Show, Eq)

queryFromDescription :: String -> Query
queryFromDescription d =
  Query { queryId          = Nothing
        , queryDescription = Just . Description $ T.pack d
        , queryIdentity    = Nothing
        , queryMeta        = Nothing
        }

queryIsEmpty :: Query -> Bool
queryIsEmpty (Query Nothing Nothing Nothing Nothing) = True
queryIsEmpty _                                       = False
