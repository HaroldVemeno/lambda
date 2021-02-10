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
  deriving (Show, Eq)

data Context = Context
  { quit :: Bool,
    names :: Map String Expr,
    maxReductions :: Int,
    reduceStepSize :: Int,
    maxSizeRel :: Int,
    maxSizeAbs :: Int,
    tryEta :: Bool
  }
  deriving (Show)

defaultContext :: Context
defaultContext =
  Context
    { quit = False,
      names = empty,
      maxReductions = 800,
      reduceStepSize = 3,
      maxSizeAbs = 4000,
      maxSizeRel = 100,
      tryEta = True
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