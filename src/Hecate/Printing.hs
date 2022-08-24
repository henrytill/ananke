module Hecate.Printing
  ( prettyResponse
  , prettyError
  ) where

import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Text.PrettyPrint         (Doc, empty, text, ($$), (<+>))

import           Hecate.Data
import           Hecate.Error             (AppError (..))
import           Hecate.Evaluator         (Response (..), Verbosity (..))


prettyText :: T.Text -> Doc
prettyText = text . T.unpack

prettyId :: Id -> Doc
prettyId i = prettyText (unId i)

prettyTimestamp :: UTCTime -> Doc
prettyTimestamp t = prettyText (showTime t)
  where
    showTime :: UTCTime -> T.Text
    showTime = T.pack . iso8601Show

prettyDescription :: Description -> Doc
prettyDescription (MkDescription d) = prettyText d

prettyIdentity :: Maybe Identity -> Doc
prettyIdentity (Just (MkIdentity i)) = prettyText i
prettyIdentity Nothing               = text "<none>"

prettyPlaintext :: Plaintext -> Doc
prettyPlaintext (MkPlaintext t) = prettyText t

prettyMeta :: Maybe Metadata -> Doc
prettyMeta (Just (MkMetadata m)) = prettyText m
prettyMeta Nothing               = text "<none>"

printPlain :: DisplayEntry -> Doc
printPlain ent = prettyPlaintext (displayPlaintext ent)

printOne :: DisplayEntry -> Doc
printOne ent =
  prettyDescription (displayDescription ent) <+>
  prettyIdentity    (displayIdentity    ent) <+>
  prettyPlaintext   (displayPlaintext   ent) <+>
  prettyMeta        (displayMeta        ent)

printOneVerbose :: DisplayEntry -> Doc
printOneVerbose ent =
  prettyId          (displayId          ent) <+>
  prettyTimestamp   (displayTimestamp   ent) <+>
  prettyDescription (displayDescription ent) <+>
  prettyIdentity    (displayIdentity    ent) <+>
  prettyPlaintext   (displayPlaintext   ent) <+>
  prettyMeta        (displayMeta        ent)

prettyResponse :: Response -> Doc
prettyResponse (SingleEntry d Normal)       = printPlain      d
prettyResponse (SingleEntry d Verbose)      = printOneVerbose d
prettyResponse (MultipleEntries [] _)       = empty
prettyResponse (MultipleEntries ds Normal)  = foldl (\acc d -> printOne        d $$ acc) empty ds
prettyResponse (MultipleEntries ds Verbose) = foldl (\acc d -> printOneVerbose d $$ acc) empty ds
prettyResponse CheckedForMultipleKeys       = text "All entries have the same keyid"
prettyResponse _                            = empty

prettyError :: AppError -> Doc
prettyError e = text (show e)
