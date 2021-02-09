module Types where

import Data.Map (Map, empty)
import Text.Parsec.Error (ParseError)

data Expr
  = Var Char
  | Abstr Char Expr
  | Appl Expr Expr
  | Name String
  deriving (Show, Eq)

data Command
  = CommandQuit
  | CommandLoad String
  | CommandLet String Expr
  deriving (Show, Eq)

data Context = Context
  { quit :: Bool,
    names :: Map String Expr,
    maxReductions :: Int,
    maxSizeRel :: Int,
    maxSizeAbs :: Int,
    tryEta :: Bool,
    tryEager :: Bool
  }
  deriving (Show)

defaultContext :: Context
defaultContext =
  Context
    { quit = False,
      names = empty,
      maxReductions = 100,
      maxSizeAbs = 500,
      maxSizeRel = 40,
      tryEta = True,
      tryEager = True
    }

data Statement
  = Command Command
  | Expr Expr
  deriving (Show, Eq)

data Error
  = ParseError ParseError
  | ReduceError String
  | ReplError String
  | TestError String
  deriving (Show, Eq)

type Result a = Either Error a