{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hecate.Client.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Error (CryptoError (..))
import Crypto.Random.Entropy (getEntropy)
import Data.Aeson (ToJSON (..), FromJSON (..), genericToEncoding, defaultOptions)
import GHC.Generics
import Hecate.Types
import Servant.Client (ClientEnv (..))
import qualified Data.Text as T

-- | A 'MasterPassword' is used, in conjunction with a 'Salt', to either
-- generate or verify a user's 'MasterKey'
newtype MasterPassword = MasterPassword T.Text
  deriving (Generic, Show)

instance ToJSON MasterPassword where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MasterPassword

-- | A 'MasterKey' is a user's key, which is generated by calling
-- 'generateMasterKey' with a 'MasterPassword' and a 'Salt'.
newtype MasterKey = MasterKey ByteString64
  deriving (Generic, Show, Eq)

instance ToJSON MasterKey where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MasterKey

-- | A 'Salt' is a value that is used, in conjunction with a 'MasterPassword',
-- to generate a 'MasterKey'
newtype Salt = Salt ByteString64
  deriving (Generic, Show, Eq)

instance ToJSON Salt where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Salt

makeSalt :: MonadIO m => Int -> m Salt
makeSalt len = Salt . ByteString64 <$> liftIO (getEntropy len)

-- | An 'Auth' is a record containing a 'MasterKey' and the 'Salt' that was used
-- to generate it
data Auth = Auth
  { key  :: MasterKey
  , salt :: Salt
  } deriving (Generic, Show, Eq)

instance ToJSON Auth where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Auth

-- | A 'PlainText' represents a decrypted value
newtype PlainText = PlainText T.Text
  deriving (Generic, Show, Eq)

instance ToJSON PlainText where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PlainText

-- | 'ClientError' represents errors that occur on the client
data ClientError
  = Base64Decoding String
  | JsonDecoding String
  | Crypto CryptoError
  | AuthVerification String
  | Integrity String
  | FileSystem String
  deriving (Show, Eq)

data ClientContext = ClientContext
  { _clientEnv :: ClientEnv
  , _authFile  :: FilePath
  }

type ClientStack a = ReaderT ClientContext (ExceptT ClientError IO) a

newtype ClientApp a = ClientApp { unClientApp :: ClientStack a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ClientError
           , MonadIO
           , MonadReader ClientContext
           )

runClientApp
  :: ClientContext
  -> ClientApp a
  -> IO (Either ClientError a)
runClientApp ctx = runExceptT . flip runReaderT ctx . unClientApp
