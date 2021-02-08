{-# LANGUAGE FlexibleContexts #-}

module Parse (parseExpr, parseStmt) where

import Control.Applicative (liftA2)
import Data.Bifunctor
import Data.Functor
import Text.Parsec
import Text.Parsec.String
import Types

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
expr = abstr <|> try appl <|> parened <|> try name <|> var

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
appl =
  try (Appl <$> applExpr <* ws <*> parened)
    <|> f <$> applExpr <* ws <*> expr
  where
    f :: Expr -> Expr -> Expr
    f a (Appl b c) = Appl (f a b) c
    f a b = Appl a b

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

{-
>>> testParse
Right True

-}

testParse :: Result Bool
testParse =
  doTest
    "Unbound varialble test"
    "a"
    (Var 'a')
    &&& doTest
      "Application test"
      "bc"
      (Appl (Var 'b') (Var 'c'))
    &&& doTest
      "Abstraction test"
      "\\p.p"
      (Abstr 'p' (Var 'p'))
    &&& doTest
      "Abstraction test 2"
      "\\pqr.p"
      (Abstr 'p' (Abstr 'q' (Abstr 'r' (Var 'p'))))
    &&& doTest
      "Parenthesis test"
      "((h)((p)))"
      (Appl (Var 'h') (Var 'p'))
    &&& doTest
      "Unbound varialble whitespace test"
      " a "
      (Var 'a')
    &&& doTest
      "Application whitespace test"
      " b c "
      (Appl (Var 'b') (Var 'c'))
    &&& doTest
      "Abstrastion whitespace test"
      " \\ p  . p "
      (Abstr 'p' (Var 'p'))
    &&& doTest
      "Parenthesis whitespace test"
      "( (  h ) (  ( p   ) )  ) "
      (Appl (Var 'h') (Var 'p'))
    &&& doTest
      "mix test 1"
      "(\\ab.b\\ca.dacb)\\po.gho"
      ( Appl
          ( Abstr
              'a'
              ( Abstr
                  'b'
                  ( Appl
                      (Var 'b')
                      ( Abstr
                          'c'
                          ( Abstr
                              'a'
                              ( Appl
                                  ( Appl
                                      ( Appl
                                          (Var 'd')
                                          (Var 'a')
                                      )
                                      (Var 'c')
                                  )
                                  (Var 'b')
                              )
                          )
                      )
                  )
              )
          )
          ( Abstr
              'p'
              ( Abstr
                  'o'
                  ( Appl
                      ( Appl
                          (Var 'g')
                          (Var 'h')
                      )
                      (Var 'o')
                  )
              )
          )
      )
    &&& doTestC
      "quit command test"
      "quit"
      CommandQuit
    &&& doTestC
      "let command test"
      "let 5 \\fx.f(f(f(f(fx))))"
      ( CommandLet
          "5"
          ( Abstr
              'f'
              ( Abstr
                  'x'
                  ( Appl
                      (Var 'f')
                      ( Appl
                          (Var 'f')
                          ( Appl
                              (Var 'f')
                              ( Appl
                                  (Var 'f')
                                  ( Appl
                                      (Var 'f')
                                      (Var 'x')
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
    &&& doTestC 
      "load command test"
      "load C:\\stuff\\haskell\\lambda.txt"
      (CommandLoad " C:\\stuff\\haskell\\lambda.txt")

  where
    doTest c str r = doTestS c str (Expr r)
    doTestC c str r = doTestS c str (Command r)
    doTestS c str r =
      let par = parseStmt c str
          same = (== r) <$> par
          un (Right s) = s
       in if same == Right False
            then
              Left $
                TestError
                  ( "Failed " ++ c
                      ++ "\nInput: "
                      ++ str
                      ++ "\nOutput: "
                      ++ show (un par)
                      ++ "\nExpected: "
                      ++ show r
                  )
            else same
    infixl 2 &&&
    (&&&) = liftA2 (&&)
