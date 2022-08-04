module Hecate.Printing
  ( prettyResponse
  , prettyError
  ) where

import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime)
import qualified Data.Time.Format        as Format
import           Prelude                 hiding ((<$>))
import           Text.PrettyPrint.Leijen (Doc, (<$>), (<+>))
import qualified Text.PrettyPrint.Leijen as Leijen

import           Hecate.Data
import           Hecate.Error            (AppError (..))
import           Hecate.Evaluator        (Command, Response (..), Verbosity (..))


prettyText :: T.Text -> Doc
prettyText = Leijen.text . T.unpack

prettyId :: Id -> Doc
prettyId i = prettyText (unId i)

prettyTimestamp :: UTCTime -> Doc
prettyTimestamp t = prettyText (showTime t)
  where
    showTime :: UTCTime -> T.Text
    showTime = T.pack . Format.formatTime Format.defaultTimeLocale "%c"

prettyDescription :: Description -> Doc
prettyDescription (Description d) = prettyText d

prettyIdentity :: Maybe Identity -> Doc
prettyIdentity (Just (Identity i)) = prettyText i
prettyIdentity Nothing             = Leijen.text "<none>"

prettyPlaintext :: Plaintext -> Doc
prettyPlaintext (Plaintext t) = prettyText t

prettyMeta :: Maybe Metadata -> Doc
prettyMeta (Just (Metadata m)) = prettyText m
prettyMeta Nothing             = Leijen.text "<none>"

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

prettyResponse :: Command -> Response -> Doc
prettyResponse _ (SingleEntry de Normal) =
  printPlain de <> Leijen.linebreak
prettyResponse _ (SingleEntry de Verbose) =
  printOneVerbose de <> Leijen.linebreak
prettyResponse _ (MultipleEntries [] _) =
  Leijen.empty
prettyResponse _ (MultipleEntries ds Normal) =
  foldl (\ acc b -> printOne b <$> acc) Leijen.empty ds
prettyResponse _ (MultipleEntries ds Verbose) =
  foldl (\ acc b -> printOneVerbose b <$> acc) Leijen.empty ds
prettyResponse _ Added =
  Leijen.text "Added" <> Leijen.linebreak
prettyResponse _ Exported =
  Leijen.text "Exported" <> Leijen.linebreak
prettyResponse _ Modified =
  Leijen.text "Modified" <> Leijen.linebreak
prettyResponse _ Redescribed =
  Leijen.text "Redescribed" <> Leijen.linebreak
prettyResponse _ Removed =
  Leijen.text "Removed" <> Leijen.linebreak
prettyResponse _ CheckedForMultipleKeys =
  Leijen.text "All entries have the same keyid" <> Leijen.linebreak

prettyError :: Command -> AppError -> Doc
prettyError _ e = Leijen.text (show e) <> Leijen.linebreak
