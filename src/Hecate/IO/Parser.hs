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

importParser :: Parser Command
importParser = Import <$> pathParser
  where
    pathParser = argument str $ metavar "PATH"
                             <> help "Path of CSV file"

cmdAdd    :: Mod CommandFields Command
cmdRemove :: Mod CommandFields Command
cmdLookup :: Mod CommandFields Command
cmdImport :: Mod CommandFields Command
cmdAdd    = command "add"    $ info addParser    (progDesc "Encrypt a piece of text and add it to the store")
cmdRemove = command "rm"     $ info removeParser (progDesc "Remove a piece of encrypted text from the store")
cmdLookup = command "lookup" $ info lookupParser (progDesc "Lookup a piece of encrypted text from the store")
cmdImport = command "import" $ info importParser (progDesc "Import a CSV file")

master :: Parser Command
master = hsubparser (cmdAdd <> cmdRemove <> cmdLookup <> cmdImport)

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A simple password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
