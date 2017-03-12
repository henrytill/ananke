{-# LANGUAGE RecordWildCards #-}

module Hecate.Printing
  ( ansiPrettyResponse
  , prettyResponse
  , prettyError
  ) where

import Hecate.Types
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T

prettyText :: T.Text -> Doc
prettyText = text . T.unpack

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
printOne DisplayEntry{..} =
  prettyDescription displayDescription <+>
  prettyIdentity    displayIdentity    <+>
  prettyPlaintext   displayPlaintext   <+>
  prettyMeta        displayMeta

prettyResponse :: Command -> Response -> Doc
prettyResponse _ (SingleEntry de) =
  printOne de <> linebreak
prettyResponse _ (MultipleEntries []) =
  text "Not found" <> linebreak
prettyResponse _ (MultipleEntries ds) =
  foldl (\ acc b -> printOne b <$> acc) empty ds
prettyResponse _ Added =
  text "Added" <> linebreak
prettyResponse _ Removed =
  text "Removed" <> linebreak

ansiPrettyResponse :: Command -> Response -> Doc
ansiPrettyResponse _ (SingleEntry de) =
  printOne de <> linebreak
ansiPrettyResponse _ (MultipleEntries []) =
  red (text "Not found") <> linebreak
ansiPrettyResponse _ (MultipleEntries ds) =
  foldl (\ acc b -> printOne b <$> acc) empty ds
ansiPrettyResponse _ Added =
  green (text "Added") <> linebreak
ansiPrettyResponse _ Removed =
  green (text "Removed") <> linebreak

prettyError :: Command -> AppError -> Doc
prettyError _ e = text (show e)
