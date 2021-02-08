{-# LANGUAGE FlexibleContexts #-}

module Parse (parseExpr, parseStmt, parseInput) where

import Control.Applicative (liftA2)
import Data.Bifunctor
import Data.Functor
import Text.Parsec
import Text.Parsec.String
import Types


parseStmt :: SourceName -> String -> Result Statement
parseStmt name str = case parse start name str of
  Right t -> Right t
  Left pe -> Left $ ParseError pe

parseExpr :: SourceName -> String -> Result Expr
parseExpr name str = case parse startExpr name str of
  Right t -> Right t
  Left pe -> Left $ ParseError pe

ws :: Parser ()
ws = spaces

start :: Parser Statement
start = try (Command <$> startCommand) <|> Expr <$> startExpr

startExpr :: Parser Expr
startExpr = ws *> expr <* ws <* eof

startCommand :: Parser Command
startCommand = ws *> command <* ws <* eof

expr :: Parser Expr
expr = try appl <|> abstr <|> parened <|> try name <|> var

-- abstr = f <$ char '\\' <* ws <*> sepBy1 lower ws <* ws <* char '.' <* ws <*> pExpr
abstr :: Parser Expr
abstr = do
  char '\\'
  ws
  c <- many1 (Just <$> letter <* ws)
  char '.'
  ws
  f c <$> expr
  where
    f :: [Maybe Char] -> Expr -> Expr
    f [] e = e
    f (Nothing : xs) e = f xs e
    f ((Just x) : xs) e = Abstr x $ f xs e

var :: Parser Expr
var = Var <$> lower

appl :: Parser Expr
appl = foldl1 Appl <$> many1 applExpr
  where
    applExpr :: Parser Expr
    applExpr = parened <|> abstr <|> try name <|> var

parened :: Parser Expr
parened = char '(' *> ws *> expr <* ws <* char ')'

name :: Parser Expr
name = Name <$> nameStr

nameStr :: Parser String
nameStr = (:) <$> (upper <|> digit) <*> many alphaNum

parseInput :: IO ()
parseInput = getLine >>= parseTest start

command :: Parser Command
command =
  try (CommandLet <$ string "let" <* ws <*> nameStr <* ws <*> expr)
    <|> (CommandQuit <$ string "quit")
    <|> (CommandLoad <$ string "load" <*> manyTill anyChar eof)