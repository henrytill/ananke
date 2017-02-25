module Hecate.IO.Parser (runCLIParser) where

import Data.Monoid ((<>))
import Hecate.Types
import Options.Applicative

addParser :: Parser Command
addParser = Add <$> descParser
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

removeParser :: Parser Command
removeParser = Remove <$> descParser
  where
    descParser = argument str $ metavar "DESCRIPTION"
                             <> help "Description of encrypted text to remove"

lookupParser :: Parser Command
lookupParser = Lookup <$> descParser
  where
    descParser = argument str $ metavar "DESCRIPTION"
                             <> help "Description of encrypted text to lookup"

cmdAdd    :: Mod CommandFields Command
cmdRemove :: Mod CommandFields Command
cmdLookup :: Mod CommandFields Command
cmdAdd    = command "add"    $ info addParser    (progDesc "Encrypt a piece of text and add it to the store")
cmdRemove = command "rm"     $ info removeParser (progDesc "Remove a piece of encrypted text from the store")
cmdLookup = command "lookup" $ info lookupParser (progDesc "Lookup a piece of encrypted text from the store")

master :: Parser Command
master = hsubparser (cmdAdd <> cmdRemove <> cmdLookup)

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A simple password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
