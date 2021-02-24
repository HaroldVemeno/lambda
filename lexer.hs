module Lexer (Token (..), TokenPos, token, tokens, tokenize) where

import Control.Monad
import Print
import Text.Parsec hiding (token, tokens)
import Text.Parsec.String
import Types hiding (Expr (..))

data Token
  = NAME String
  | VAR Char
  | OP
  | CP
  | BS
  | DOT
  | KW String
  | LOAD String
  | SET String String
  deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

ws, ws', sp, sp' :: Parser ()
ws = spaces
ws' = void $ many1 space
sp = void . many $ oneOf " \f\t\v"
sp' = void . many1 $ oneOf " \f\t\v"

posify :: Parser Token -> Parser TokenPos
posify p = do
  pos <- getPosition
  t <- p
  return (t, pos)

nameStr :: Parser String
nameStr = (:) <$> (upper <|> digit <|> oneOf "-_'") <*> many (alphaNum <|> oneOf "-_'")

name, var, op, cp, bs, dot, kw, load, set :: Parser Token
name = NAME <$> nameStr
var = VAR <$> lower
op = OP <$ char '('
cp = CP <$ char ')'
bs = BS <$ char '\\'
dot = DOT <$ char '.'
load = LOAD <$ try (string "Load") <* sp' <*> many1 anyChar <* sp <* (eof <|> void newline)
set = SET <$ try (string "Set") <* sp' <*> many1 letter <* sp' <*> many1 anyChar <* (eof <|> void newline)
kw = KW <$> choice [s "Let", s "Tree", s "Quit", (:) <$> char 'S' <*> choice [s "tep", s "how", s "kify"]] <?> "keyword"
  where
    s = string

token :: Parser TokenPos
token = choice (posify <$> [load, set, try kw, var, name, op, cp, bs, dot]) <?> "token start"

tokens :: Parser [TokenPos]
tokens = ws *> many (token <* ws) <* eof

tokenize :: SourceName -> String -> Result [TokenPos]
tokenize sn str = case parse tokens (sn ++ " (tokenization)") str of
  Right a -> Right a
  Left err -> Left (ParseError err)