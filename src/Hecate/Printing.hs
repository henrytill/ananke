module Hecate.Printing
  ( prettyResponse
  , prettyError
  , render
  ) where

import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Text.PrettyPrint         (Doc, Mode (..), Style (..), empty, renderStyle, style, text, ($$), (<+>))

import           Hecate.Data
import           Hecate.Error             (AppError (..))
import           Hecate.Evaluator         (Response (..), Verbosity (..))


prettyText :: T.Text -> Doc
prettyText = text . T.unpack

prettyId :: Id -> Doc
prettyId = prettyText . unId

prettyTimestamp :: UTCTime -> Doc
prettyTimestamp = text . iso8601Show

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
printPlain = prettyPlaintext . displayPlaintext

printOne :: DisplayEntry -> Doc
printOne ent =
  prettyDescription (displayDescription ent) <+>
  prettyIdentity    (displayIdentity    ent) <+>
  prettyPlaintext   (displayPlaintext   ent)

printOneVerbose :: DisplayEntry -> Doc
printOneVerbose ent =
  prettyId          (displayId          ent) <+>
  prettyTimestamp   (displayTimestamp   ent) <+>
  prettyDescription (displayDescription ent) <+>
  prettyIdentity    (displayIdentity    ent) <+>
  prettyPlaintext   (displayPlaintext   ent) <+>
  prettyMeta        (displayMeta        ent)

prettyResponse :: Response -> Doc
prettyResponse (SingleEntry     d   Normal) = printPlain      d
prettyResponse (SingleEntry     d  Verbose) = printOneVerbose d
prettyResponse (MultipleEntries []       _) = empty
prettyResponse (MultipleEntries ds  Normal) = foldl (\acc d -> printOne        d $$ acc) empty ds
prettyResponse (MultipleEntries ds Verbose) = foldl (\acc d -> printOneVerbose d $$ acc) empty ds
prettyResponse CheckedForMultipleKeys       = text "All entries have the same keyid"
prettyResponse _                            = empty

prettyError :: AppError -> Doc
prettyError = text . show

render :: Doc -> String
render = renderStyle style{mode = LeftMode}
