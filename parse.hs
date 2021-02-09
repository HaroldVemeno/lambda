{-# LANGUAGE FlexibleContexts #-}

module Parse (parseExpr, parseStmt, parseInput) where

import Control.Applicative (liftA2)
import Data.Bifunctor
import Data.Functor
import Text.Parsec
import Text.Parsec.String
import Types

parseBy :: Parser a -> SourceName -> String -> Result a
parseBy par name str = case parse par name str of
  Right t -> Right t
  Left pe -> Left $ ParseError pe

parseStmt :: SourceName -> String -> Result Statement
parseStmt = parseBy start

parseExpr :: SourceName -> String -> Result Expr
parseExpr = parseBy startExpr

ws :: Parser ()
ws = spaces

ws' :: Parser ()
ws' = space *> ws

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
appl = foldl1 Appl <$> many1 (applExpr <* ws)
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
  try (CommandLet <$ string "let" <* ws' <*> nameStr <* ws <*> expr)
    <|> (CommandQuit <$ string "quit")
    <|> (CommandLoad <$ string "load" <* ws' <*> manyTill anyChar eof)