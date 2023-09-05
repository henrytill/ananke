{-# LANGUAGE CPP #-}

module Hecate.Parser
  ( runCLIParser
  ) where

import           Options.Applicative

import           Hecate.Data
import           Hecate.Evaluator


descArgP :: Parser Description
descArgP = MkDescription <$> argument str (metavar "DESC" <> help "Description of ciphertext")

hashOptP :: Parser Id
hashOptP = MkId <$> strOption def
  where def = long "hash"
              <> short 'h'
              <> metavar "HASH"
              <> help "SHA1 Hash of desired entry"

descOptP :: Parser Description
descOptP = MkDescription <$> strOption def
  where def = long "description"
              <> short 'd'
              <> metavar "DESC"
              <> help "Description of desired entry"

idenOptP :: Parser Identity
idenOptP = MkIdentity <$> strOption def
  where def = long "identity"
              <> short 'i'
              <> metavar "ID"
              <> help "Identity associated with ciphertext"

metaOptP :: Parser Metadata
metaOptP = MkMetadata <$> strOption def
  where def = long "metadata"
              <> short 'm'
              <> metavar "META"
              <> help "Metadata associated with ciphertext"

targetP :: Parser Target
targetP = (TargetId <$> hashOptP) <|> (TargetDescription <$> descOptP)

modifyCiphertextFlagP :: Parser ModifyAction
verbosityFlagP        :: Parser Verbosity
modifyCiphertextFlagP = flag Keep   Change  $ long "ciphertext" <> short 'c' <> help "Modify ciphertext"
verbosityFlagP        = flag Normal Verbose $ long "verbose"    <> short 'v' <> help "Display verbose results"

addP        :: Parser Command
lookupP     :: Parser Command
modifyP     :: Parser Command
redescribeP :: Parser Command
removeP     :: Parser Command
checkP      :: Parser Command
addP        = Add        <$> descArgP <*> optional idenOptP     <*> optional metaOptP
lookupP     = Lookup     <$> descArgP <*> optional idenOptP     <*> verbosityFlagP
modifyP     = Modify     <$> targetP  <*> modifyCiphertextFlagP <*> optional idenOptP <*> optional metaOptP
redescribeP = Redescribe <$> targetP  <*> descArgP
removeP     = Remove     <$> targetP
checkP      = pure CheckForMultipleKeys

cmdAdd        :: Mod CommandFields Command
cmdLookup     :: Mod CommandFields Command
cmdModify     :: Mod CommandFields Command
cmdRedescribe :: Mod CommandFields Command
cmdRemove     :: Mod CommandFields Command
cmdCheck      :: Mod CommandFields Command
cmdAdd        = command "add"        . info addP        $ progDesc "Encrypt a piece of text and add it to the store"
cmdLookup     = command "lookup"     . info lookupP     $ progDesc "Lookup a piece of ciphertext in the store"
cmdModify     = command "modify"     . info modifyP     $ progDesc "Modify a piece of ciphertext in the store"
cmdRedescribe = command "redescribe" . info redescribeP $ progDesc "Modify the description of a piece of ciphertext in the store"
cmdRemove     = command "remove"     . info removeP     $ progDesc "Remove a piece of ciphertext from the store"
cmdCheck      = command "check"      . info checkP      $ progDesc "Check if all entries have the same keyid"

#ifdef BACKEND_JSON
pathArgP :: Parser FilePath
pathArgP = argument str $ metavar "PATH" <> help "Path of JSON file"

importP :: Parser Command
importP = Import <$> pathArgP

exportP :: Parser Command
exportP = Export <$> pathArgP

cmdImport :: Mod CommandFields Command
cmdExport :: Mod CommandFields Command
cmdImport = command "import" . info importP $ progDesc "Import a JSON file"
cmdExport = command "export" . info exportP $ progDesc "Export a JSON file"

master :: Parser Command
master = hsubparser (cmdAdd <> cmdLookup <> cmdImport <> cmdExport <> cmdModify <> cmdRedescribe <> cmdRemove <> cmdCheck)
#else
master :: Parser Command
master = hsubparser (cmdAdd <> cmdLookup <> cmdModify <> cmdRedescribe <> cmdRemove <> cmdCheck)
#endif

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A minimal password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
