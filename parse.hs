--{-# LANGUAGE FlexibleContexts #-}
module Parse (parseExpr, parseStmt, parseFile) where

import Control.Applicative (liftA2)
import Text.Parsec hiding (token, tokens, satisfy)

import Types
import Lexer

type Parser a = Parsec [TokenPos] () a

rawParse :: Parser a -> SourceName -> [TokenPos] -> Result a
rawParse par name i = case parse par (name ++ " (parsing)") i of
  Right t -> Right t
  Left pe -> Left $ ParseError pe

parseBy :: Parser a -> SourceName -> String -> Result a
parseBy par name str = rawParse par name =<< tokenize name str 

parseStmt :: SourceName -> String -> Result Statement
parseStmt = parseBy startStmt

parseExpr :: SourceName -> String -> Result Expr
parseExpr = parseBy startExpr

startStmt :: Parser Statement
startStmt = statement <* eof

startExpr :: Parser Expr
startExpr = expr <* eof

statement :: Parser Statement
statement = try (Command <$> command) <|> Expr <$> expr

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = tokenPrim show
                      advance
                      (\c -> if f (fst c) then Just (fst c) else Nothing)

tok :: Token -> Parser Token
tok t = satisfy (== t) <?> show t

var :: Parser Expr
var = to <$> satisfy is <?> "Var" 
  where
    is x = case x of { VAR a -> True; _ -> False}
    to (VAR x) = Var x


name :: Parser Expr
name = to <$> satisfy is <?> "Name" 
  where
    is x = case x of { NAME a -> True; _ -> False}
    to (NAME x) = Name x

expr :: Parser Expr
expr = foldl1 Appl <$> many1 applExpr
  where
    applExpr :: Parser Expr
    applExpr = parened <|> abstr <|> var <|> name

-- abstr = f <$ char '\\' <* ws <*> sepBy1 lower ws <* ws <* char '.' <* ws <*> pExpr
abstr :: Parser Expr
abstr = do
  tok BS
  c <- many1 var
  tok DOT
  f c <$> expr
  where
    f :: [Expr] -> Expr -> Expr
    f l e = foldr (\(Var a) e -> Abstr a e) e l
    --f ((Var a):as) e = Abstr a $ f as e  
    --f [] e = e

parened :: Parser Expr
parened = tok OP *> expr <* tok CP

load :: Parser Command
load = to <$> satisfy is <?> "Load x" 
  where
    is x = case x of { LOAD a -> True; _ -> False}
    to (LOAD x) = CommandLoad x

command :: Parser Command
command =
  try (CommandLet <$ kw "Let" <*> nameStr <*> expr)
    <|> load
    <|> (CommandQuit <$ kw "Quit")
    <|> (CommandTree <$ kw "Tree" <*> expr)
    <|> (CommandStep <$ kw "Step" <*> expr)
  where 
    kw n = tok $ KW n
    nameStr = (\(Name n) -> n) <$> name

parseFile :: SourceName -> String -> Result [Command]
parseFile = parseBy (many command <* eof)