module Hecate.Parser
  ( runCLIParser
  ) where

import Data.Monoid ((<>))
import Options.Applicative

import Hecate.Evaluator (Command(..), Verbosity(..), Removal(..))


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
removeParser = Remove <$> removalParser
  where
    removalParser = (RemoveId <$> hashParser) <|> (RemoveDescription <$> descParser)

    hashParser = strOption $ long "hash"
                          <> short 'h'
                          <> metavar "HASH"
                          <> help "SHA1 Hash of entry to remove"

    descParser = strOption $ long "description"
                          <> short 'd'
                          <> metavar "DESCRIPTION"
                          <> help "Description of entry to remove"

lookupParser :: Parser Command
lookupParser = Lookup <$> descParser
                      <*> verbosityFlag
  where
    descParser = argument str $ metavar "DESCRIPTION"
                             <> help "Description of encrypted text to lookup"

    verbosityFlag = flag Normal Verbose $ long "verbose"
                                       <> short 'v'
                                       <> help "Display verbose results"

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
opts = info (master <**> helper) (fullDesc <> progDesc "A minimal password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
