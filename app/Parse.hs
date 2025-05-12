module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import Control.Monad
import Debug.Trace

import Syntax

type Parser = Parsec Void String

keywords = ["definition", "theorem", "section", "constant", "axiom", "end", "by", "intro", "apply", "admit", "text"]

nameCont = many (lowerChar <|> upperChar)

atomic :: [String] -> Parser Term
atomic bs =
  label "variable" (do
    c <- upperChar <|> lowerChar
    cs <- nameCont
    let name = c:cs
    if isUpper c then
      pure (Con name)
    else case index name bs of
      Just ix -> pure (Var ix)
      Nothing -> fail ("Unbound variable: " ++ name)) <|>
  (do
    char '('; space
    e <- term bs; space
    char ')'
    pure e)

lambda :: [String] -> Parser Term
lambda bs = do
  string "\\"; space
  xs <- some ((:) <$> lowerChar <*> nameCont); space
  string "."; space
  e <- term (reverse xs ++ bs)
  pure (foldr Lam e xs)

term :: [String] -> Parser Term
term bs = label "term" (
  lambda bs <|>
  (do
    f <- atomic bs
    es <- many do
      forM_ keywords \kw -> notFollowedBy (space *> string kw)
      space
      atomic bs
    space
    -- lam <- try (lambda bs >>= \e -> pure [e]) <|> pure []
    pure (foldl App f es)))

source :: [String] -> Parser Source
source hs =
  label "reference" (do
    c <- upperChar <|> lowerChar
    cs <- nameCont
    let name = c:cs
    if isUpper c then
      pure (Cite name)
    else case index name hs of
      Just ix -> pure (Assume ix)
      Nothing -> fail ("Unbound hypothesis: " ++ name))

proof :: [String] -> [String] -> Parser (Proof ())
proof bs hs =
  label "introduction" (do
    string "intro"; space
    char '('
    xs <- many (space *> some lowerChar); space
    char ')'; space
    char '['
    ys <- many (space *> some lowerChar); space
    char ']'; space
    p <- proof (reverse xs ++ bs) (reverse ys ++ hs)
    pure (Intro () xs hs p)) <|>
  label "application" (do
    string "apply"; space
    src <- source hs; space
    es <- many do
      forM_ keywords \kw -> notFollowedBy (space *> string kw)
      space
      atomic bs
    space
    ps <- many (string "by" *> space *> proof bs hs <* space <* string "end")
    pl <- try (space *> proof bs hs >>= \p -> pure [p]) <|> pure []
    pure (Apply () src es (ps ++ pl))) <|>
  label "admit" (do
    string "admit"
    pure (Admit ()))

block :: Parser Block
block =
  label "prose" (do
    string "text"
    cs <- many (notFollowedBy (string "end") *> anySingle)
    string "end"
    pure (Prose cs)) <|>
  label "inclusion" (do
    string "include"; space
    name <- (:) <$> upperChar <*> nameCont
    pure (Include name))

object :: Parser (Object ())
object =
  label "definition" (do
    string "definition"; space
    char '"'
    name <- many (notFollowedBy (char '"') *> anySingle)
    char '"'; space
    e <- term []; space
    string "end"
    pure (Definition name e)) <|>
  label "theorem" (do
    string "theorem"; space
    char '"'
    name <- many (notFollowedBy (char '"') *> anySingle)
    char '"'; space
    e <- term []; space
    string "by"; space
    p <- proof [] []; space
    string "end"
    pure (Theorem name e p)) <|>
  label "section" (do
    string "section"; space
    char '"'
    name <- many (notFollowedBy (char '"') *> anySingle)
    char '"'
    bs <- many (notFollowedBy (space *> string "end") *> space *> block); space
    string "end"
    pure (Section name bs)) <|>
  label "constant" (do
    string "constant"; space
    char '"'
    name <- many (notFollowedBy (char '"') *> anySingle)
    char '"'
    pure (Constant name)) <|>
  label "axiom" (do
    string "axiom"; space
    char '"'
    name <- many (notFollowedBy (char '"') *> anySingle)
    char '"'; space
    e <- term []
    pure (Axiom name e))