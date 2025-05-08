module Parse where

import Term

import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

import Data.Void

type Parser = Parsec Void String

name :: Parser String
name = some alphaNumChar

appe f = foldl App f

index :: String -> [String] -> Int
index s [] = error $ "no index " ++ s
index s (x:_) | x == s = 0
index s (_:xs) = index s xs + 1

atomic :: [String] -> Parser Term
atomic bs =
  try (do
    char '('
    e <- term bs
    char ')'
    pure e) <|>
  try (do
    n <- name
    pure (Var (index n bs))) <|>
  (do
    char ':'
    n <- name
    pure (Con (ID n)))

term :: [String] -> Parser Term
term bs =
  try (do
    n <- atomic bs
    args <-
      try (do
        char ' '
        es <- sepBy (atomic bs) (string " ")
        pure es) <|>
      pure []
    pure (appe n args)) <|>
  (do
    string "|"
    n <- name
    string "| "
    e <- term (n:bs)
    pure (Lam n e))

pterm :: String -> Term
pterm r = case parse (term [] <* eof) "" r of
  Right e -> e
  Left e -> error $ errorBundlePretty e