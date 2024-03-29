module Ananke.Printing
  ( prettyResponse,
    prettyError,
    render,
  )
where

import Ananke.Data
import Ananke.Error (AppError (..))
import Ananke.Evaluator (Response (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.PrettyPrint (Doc, Mode (..), Style (..), empty, renderStyle, style, text, ($$), (<+>))

prettyText :: Text -> Doc
prettyText = text . Text.unpack

prettyTimestamp :: UTCTime -> Doc
prettyTimestamp = text . iso8601Show

prettyId :: Id -> Doc
prettyId = prettyText . unId

prettyKeyId :: KeyId -> Doc
prettyKeyId (MkKeyId k) = prettyText k

prettyDescription :: Description -> Doc
prettyDescription (MkDescription d) = prettyText d

prettyPlaintext :: Plaintext -> Doc
prettyPlaintext (MkPlaintext t) = prettyText t

prettyIdentity :: Maybe Identity -> Doc
prettyIdentity (Just (MkIdentity i)) = prettyText i
prettyIdentity Nothing = empty

prettyMeta :: Maybe Metadata -> Doc
prettyMeta (Just (MkMetadata m)) = prettyText m
prettyMeta Nothing = empty

printPlain :: DisplayEntry -> Doc
printPlain = prettyPlaintext . displayPlaintext

printOne :: DisplayEntry -> Doc
printOne e =
  prettyDescription (displayDescription e)
    <+> prettyIdentity (displayIdentity e)
    <+> prettyPlaintext (displayPlaintext e)

printOneVerbose :: DisplayEntry -> Doc
printOneVerbose e =
  prettyTimestamp (displayTimestamp e)
    <+> prettyId (displayId e)
    <+> prettyKeyId (displayKeyId e)
    <+> prettyDescription (displayDescription e)
    <+> prettyIdentity (displayIdentity e)
    <+> prettyPlaintext (displayPlaintext e)
    <+> prettyMeta (displayMeta e)

prettyResponse :: Response -> Doc
prettyResponse (SingleEntry d False) = printPlain d
prettyResponse (SingleEntry d True) = printOneVerbose d
prettyResponse (MultipleEntries [] _) = empty
prettyResponse (MultipleEntries ds False) = foldl (\acc d -> printOne d $$ acc) empty ds
prettyResponse (MultipleEntries ds True) = foldl (\acc d -> printOneVerbose d $$ acc) empty ds
prettyResponse CheckedForMultipleKeys = text "All entries have the same keyid"
prettyResponse _ = empty

prettyError :: AppError -> Doc
prettyError = text . show

render :: Doc -> String
render = renderStyle style {mode = LeftMode}
