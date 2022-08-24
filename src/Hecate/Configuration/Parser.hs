{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hecate.Configuration.Parser
  ( Pairs
  , parse
  ) where

import           Data.Char                    (isAlphaNum)
import           Data.List                    (find)
import           Text.ParserCombinators.ReadP


type Pairs = [(String, String)]

identifier :: ReadP String
identifier = munch $ \c -> isAlphaNum c || c == '-' || c == '_'

equals :: ReadP ()
equals = skipSpaces >> char '=' >> skipSpaces

pair :: ReadP (String, String)
pair = do
  k <- identifier
  equals
  v <- identifier
  return (k, v)

eol :: ReadP ()
eol = string "\n\r"
  <++ string "\r\n"
  <++ string "\n"
  <++ string "\r"
  >> return ()

comment :: ReadP ()
comment = spaces >> char '#' >> manyTill get eol >> return ()
  where
    spaces :: ReadP ()
    spaces = skipMany . satisfy $ \c -> c == ' ' || c == '\t'

pairs :: ReadP Pairs
pairs = many (comment <++ eol) >> endBy pair (many1 (comment <++ eol))

parse :: String -> Maybe Pairs
parse = fmap fst . find (null . snd) . readP_to_S pairs

-- for interactive development
parseFile :: FilePath -> IO ()
parseFile name = do
  c <- readFile name
  putStrLn "Full:"
  mapM_ print $ readP_to_S pairs c
  putStrLn "Pruned:"
  mapM_ print $ parse c
