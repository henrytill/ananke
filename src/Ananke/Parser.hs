module Ananke.Parser
  ( runCLIParser,
  )
where

import Ananke.Data
import Ananke.Evaluator
import Options.Applicative

descArgP :: Parser Description
descArgP = MkDescription <$> argument str (metavar "DESC" <> help "Description of ciphertext")

hashOptP :: Parser Id
hashOptP = MkId <$> strOption def
  where
    def =
      long "hash"
        <> short 'h'
        <> metavar "HASH"
        <> help "SHA1 Hash of desired entry"

descOptP :: Parser Description
descOptP = MkDescription <$> strOption def
  where
    def =
      long "description"
        <> short 'd'
        <> metavar "DESC"
        <> help "Description of desired entry"

idenOptP :: Parser Identity
idenOptP = MkIdentity <$> strOption def
  where
    def =
      long "identity"
        <> short 'i'
        <> metavar "ID"
        <> help "Identity associated with ciphertext"

metaOptP :: Parser Metadata
metaOptP = MkMetadata <$> strOption def
  where
    def =
      long "metadata"
        <> short 'm'
        <> metavar "META"
        <> help "Metadata associated with ciphertext"

targetP :: Parser Target
targetP = (TargetId <$> hashOptP) <|> (TargetDescription <$> descOptP)

modifyCiphertextFlagP :: Parser ModifyAction
modifyCiphertextFlagP = flag Keep Change $ long "ciphertext" <> short 'c' <> help "Modify ciphertext"

verbosityFlagP :: Parser Verbosity
verbosityFlagP = flag Normal Verbose $ long "verbose" <> short 'v' <> help "Display verbose results"

addP :: Parser Command
addP = Add <$> descArgP <*> optional idenOptP <*> optional metaOptP

lookupP :: Parser Command
lookupP = Lookup <$> descArgP <*> optional idenOptP <*> verbosityFlagP

modifyP :: Parser Command
modifyP = Modify <$> targetP <*> modifyCiphertextFlagP <*> optional idenOptP <*> optional metaOptP

redescribeP :: Parser Command
redescribeP = Redescribe <$> targetP <*> descArgP

removeP :: Parser Command
removeP = Remove <$> targetP

checkP :: Parser Command
checkP = pure CheckForMultipleKeys

cmdAdd :: Mod CommandFields Command
cmdAdd = command "add" . info addP $ progDesc d
  where
    d = "Encrypt a piece of text and add it to the store"

cmdLookup :: Mod CommandFields Command
cmdLookup = command "lookup" . info lookupP $ progDesc d
  where
    d = "Lookup a piece of ciphertext in the store"

cmdModify :: Mod CommandFields Command
cmdModify = command "modify" . info modifyP $ progDesc d
  where
    d = "Modify a piece of ciphertext in the store"

cmdRedescribe :: Mod CommandFields Command
cmdRedescribe = command "redescribe" . info redescribeP $ progDesc d
  where
    d = "Modify the description of a piece of ciphertext in the store"

cmdRemove :: Mod CommandFields Command
cmdRemove = command "remove" . info removeP $ progDesc d
  where
    d = "Remove a piece of ciphertext from the store"

cmdCheck :: Mod CommandFields Command
cmdCheck = command "check" . info checkP $ progDesc d
  where
    d = "Check if all entries have the same keyid"

pathArgP :: Parser FilePath
pathArgP = argument str $ metavar "PATH" <> help "Path of JSON file"

importP :: Parser Command
importP = Import <$> pathArgP

exportP :: Parser Command
exportP = Export <$> pathArgP

cmdImport :: Mod CommandFields Command
cmdImport = command "import" . info importP $ progDesc "Import a JSON file"

cmdExport :: Mod CommandFields Command
cmdExport = command "export" . info exportP $ progDesc "Export a JSON file"

cmd :: Parser Command
cmd =
  hsubparser
    ( cmdAdd
        <> cmdLookup
        <> cmdImport
        <> cmdExport
        <> cmdModify
        <> cmdRedescribe
        <> cmdRemove
        <> cmdCheck
    )

versioner :: Parser (a -> a)
versioner = simpleVersioner showVersion

opts :: ParserInfo Command
opts = info (cmd <**> versioner <**> helper) (fullDesc <> progDesc "A minimal password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
