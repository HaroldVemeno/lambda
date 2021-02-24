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
  | CommandTree Expr
  | CommandStep Expr
  | CommandShow Expr
  | CommandSet String String
  | CommandSkify Expr
  deriving (Show, Eq)

data Context = Context
  { quit :: Bool,
    names :: Map String Expr,
    maxReductions :: Int,
    reduceStepSize :: Int,
    maxSizeRel :: Int,
    maxSizeAbs :: Int,
    tryEta :: Bool,
    forceNames :: Bool
  }
  deriving (Show)

defaultContext :: Context
defaultContext =
  Context
    { quit = False,
      names = empty,
      maxReductions = 100000,
      reduceStepSize = 1,
      maxSizeAbs = 8000,
      maxSizeRel = 10000,
      tryEta = True,
      forceNames = False
    }

data Statement
  = Command Command
  | Expr Expr
  deriving (Show, Eq)

data Error
  = ParseError ParseError
  | ReduceError String Expr
  | ReplError String
  | TestError String
  deriving (Eq)

type Result a = Either Error a