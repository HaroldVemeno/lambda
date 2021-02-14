module Lexer where

import Control.Monad
import Print
import Text.Parsec hiding (token, tokens)
import Text.Parsec.String
import Types hiding (Expr (..))

data Token
  = Name String
  | Var Char
  | OP
  | CP
  | BS
  | Dot
  | KW String
  | Load String
  | FP String
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

name, var, op, cp, bs, dot, kw, load :: Parser Token
name = Name <$> nameStr

nameStr :: Parser String
nameStr = (:) <$> (upper <|> digit) <*> many alphaNum

var = Var <$> lower

op = OP <$ char '('

cp = CP <$ char ')'

bs = BS <$ char '\\'

dot = Dot <$ char '.'

kw = KW <$> choice (fmap string ["Let", "Tree", "Quit", "Step"]) <?> "keyword"

load = Load <$ string "Load" <* sp' <*> many1 anyChar <* sp <* (eof <|> void newline)

token :: Parser TokenPos
token = choice (posify <$> [try load, try kw, var, name, op, cp, bs, dot]) <?> "token start"

tokens :: Parser [TokenPos]
tokens = ws *> many (token <* ws) <* eof

tokenize :: SourceName -> String -> Result [TokenPos]
tokenize sn str = case parse tokens sn str of
  Right a -> Right a
  Left err -> Left (ParseError err)