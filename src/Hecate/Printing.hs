{-# LANGUAGE RecordWildCards #-}

module Hecate.Printing
  ( ppResponse
  , ppAppError
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Hecate.Types
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"

prettyText :: T.Text -> Doc
prettyText = text . T.unpack

prettyDescription :: Description -> Doc
prettyDescription (Description d) = prettyText d

prettyIdentity :: Maybe Identity -> Doc
prettyIdentity (Just (Identity i)) = prettyText i
prettyIdentity Nothing             = empty

prettyPlaintext :: Plaintext -> Doc
prettyPlaintext (Plaintext t) = prettyText t

prettyMeta :: Maybe Metadata -> Doc
prettyMeta (Just (Metadata m)) = prettyText m
prettyMeta Nothing             = empty

printOne :: DisplayEntry -> Doc
printOne DisplayEntry{..} =
  prettyDescription displayDescription <+>
  prettyIdentity    displayIdentity    <+>
  prettyPlaintext   displayPlaintext   <+>
  prettyMeta        displayMeta

ppResponse :: Command -> Response -> Doc
ppResponse _ (SingleEntry de) =
  green (text "Found:") <+> printOne de <> linebreak
ppResponse _ (MultipleEntries []) =
  red (text "Not found.") <> linebreak
ppResponse _ (MultipleEntries ds) =
  green (text "Found:") <$> foldl (\ acc b -> printOne b <$> acc) empty ds
ppResponse _ Added =
  blue (text "Added") <> linebreak
ppResponse _ Removed =
  blue (text "Removed") <> linebreak

ppAppError :: Command -> AppError -> Doc
ppAppError _ e = red (text (show e))
