{-# LANGUAGE NamedFieldPuns #-}

module Hecate.Printing
  ( ansiPrettyResponse
  , prettyResponse
  , prettyError
  ) where

import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Hecate.Data
import           Hecate.Error
import           Hecate.Evaluator
import           Hecate.GPG


prettyText :: T.Text -> Doc
prettyText = text . T.unpack

prettyId :: Id -> Doc
prettyId i = prettyText (unId i)

prettyTimestamp :: UTCTime -> Doc
prettyTimestamp t = prettyText (showTime t)
  where
    showTime :: UTCTime -> T.Text
    showTime = T.pack . formatTime defaultTimeLocale "%c"

prettyDescription :: Description -> Doc
prettyDescription (Description d) = prettyText d

prettyIdentity :: Maybe Identity -> Doc
prettyIdentity (Just (Identity i)) = prettyText i
prettyIdentity Nothing             = text "<none>"

prettyPlaintext :: Plaintext -> Doc
prettyPlaintext (Plaintext t) = prettyText t

prettyMeta :: Maybe Metadata -> Doc
prettyMeta (Just (Metadata m)) = prettyText m
prettyMeta Nothing             = text "<none>"

printOne :: DisplayEntry -> Doc
printOne DisplayEntry{_displayDescription, _displayIdentity, _displayPlaintext, _displayMeta} =
  prettyDescription _displayDescription <+>
  prettyIdentity    _displayIdentity    <+>
  prettyPlaintext   _displayPlaintext   <+>
  prettyMeta        _displayMeta

printOneVerbose :: DisplayEntry -> Doc
printOneVerbose DisplayEntry{_displayId, _displayTimestamp, _displayDescription, _displayIdentity, _displayPlaintext, _displayMeta} =
  prettyId          _displayId          <+>
  prettyTimestamp   _displayTimestamp   <+>
  prettyDescription _displayDescription <+>
  prettyIdentity    _displayIdentity    <+>
  prettyPlaintext   _displayPlaintext   <+>
  prettyMeta        _displayMeta

prettyResponse :: Command -> Response -> Doc
prettyResponse _ (SingleEntry de Normal) =
  printOne de <> linebreak
prettyResponse _ (SingleEntry de Verbose) =
  printOneVerbose de <> linebreak
prettyResponse _ (MultipleEntries [] _) =
  text "Not found" <> linebreak
prettyResponse _ (MultipleEntries ds Normal) =
  foldl (\ acc b -> printOne b <$> acc) empty ds
prettyResponse _ (MultipleEntries ds Verbose) =
  foldl (\ acc b -> printOneVerbose b <$> acc) empty ds
prettyResponse _ Added =
  text "Added" <> linebreak
prettyResponse _ Exported =
  text "Exported" <> linebreak
prettyResponse _ Modified =
  text "Modified" <> linebreak
prettyResponse _ Redescribed =
  text "Redescribed" <> linebreak
prettyResponse _ Removed =
  text "Removed" <> linebreak
prettyResponse _ CheckedForMultipleKeys =
  text "All entries have the same keyid" <> linebreak

ansiPrettyResponse :: Command -> Response -> Doc
ansiPrettyResponse _ (SingleEntry de Normal) =
  printOne de <> linebreak
ansiPrettyResponse _ (SingleEntry de Verbose) =
  printOneVerbose de <> linebreak
ansiPrettyResponse _ (MultipleEntries [] _) =
  red (text "Not found") <> linebreak
ansiPrettyResponse _ (MultipleEntries ds Normal) =
  foldl (\ acc b -> printOne b <$> acc) empty ds
ansiPrettyResponse _ (MultipleEntries ds Verbose) =
  foldl (\ acc b -> printOneVerbose b <$> acc) empty ds
ansiPrettyResponse _ Added =
  green (text "Added") <> linebreak
ansiPrettyResponse _ Exported =
  green (text "Exported") <> linebreak
ansiPrettyResponse _ Modified =
  green (text "Modified") <> linebreak
ansiPrettyResponse _ Redescribed =
  green (text "Redescribed") <> linebreak
ansiPrettyResponse _ Removed =
  green (text "Removed") <> linebreak
ansiPrettyResponse _ CheckedForMultipleKeys =
  green (text "All entries have the same keyid") <> linebreak

prettyError :: Command -> AppError -> Doc
prettyError _ e = text (show e) <> linebreak
