{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid ((<>))
import Hecate.Client
import Hecate.Client.Types
import Hecate.Types
import Hecate.Util
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client
import System.IO
import qualified Data.Text as T

data Command
  = Add { addDescription :: String
        , addIdentity    :: Maybe String
        , addMeta        :: Maybe String
        }
  deriving Show

newtype Response = Response { respEntry :: Entry } deriving Show

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushChar :: Char -> IO ()
flushChar s = putChar s >> hFlush stdout

evalCommand
  :: (MonadIO m, MonadError ClientError m, MonadReader ClientContext m)
  => Command
  -> m (Either ServantError Ok)
evalCommand a@Add{} = do
  ctx  <- ask
  _    <- liftIO $ flushStr "Enter your master password: "
  _    <- liftIO $ hSetEcho stdin False
  mps  <- liftIO getLine
  mk   <- loadAuth (MasterPassword (T.pack mps)) (_authFile ctx)
  _    <- liftIO $ hSetEcho stdin True
  _    <- liftIO $ flushChar '\n'
  _    <- liftIO $ flushStr "Enter text to encrypt: "
  pt   <- liftIO getLine
  e    <- entry mk (Description . T.pack  $  addDescription a)
                   (   Identity . T.pack <$> addIdentity a   )
                   (  PlainText . T.pack  $  pt              )
                   (   Metadata . T.pack <$> addMeta a       )
  liftIO $ runClientM (putEntry e) (_clientEnv ctx)

add :: Parser Command
add = Add <$> descParser
          <*> optional idenParser
          <*> optional metaParser
  where
    descParser = argument str $ metavar "DESCRIPTION"
                             <> help "Description of encrypted text"

    idenParser = strOption $ long "identity"
                          <> short 'i'
                          <> metavar "ID"
                          <> help "Identity associated with encrypted text"

    metaParser = strOption $ long "metadata"
                          <> short 'm'
                          <> metavar "METADATA"
                          <> help "Metadata associated with encrypted text"

cmdAdd :: Mod CommandFields Command
cmdAdd = command "add" $ info add (progDesc "Encrypt a piece of text and add it to the store")

master :: Parser Command
master = hsubparser cmdAdd

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A simple password manager")

testParser :: Command -> IO ()
testParser x@Add{} = print x

main :: IO ()
main = do
  home    <- getHome >>= maybe (error "Can't find my way HOME") pure
  manager <- newManager defaultManagerSettings
  let env = ClientEnv manager (BaseUrl Http "localhost" 8081 "")
      ctx = ClientContext env (home <> "./hecate/auth.json")
  cmd    <- execParser opts
  resp   <- runClientApp ctx (evalCommand cmd)
  print resp
