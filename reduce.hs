module Reduce where

import Data.Char (chr, ord)
import Types

dc :: Context
dc = defaultContext

basicRawReduce :: Expr -> Expr
basicRawReduce f = start f 0
  where
    l = size f
    start e n =
      let e' = basicRawReduceOnce e
          s = size e'
       in if n >= maxReductions dc || e == e' || s > (maxSizeRel dc * l) || s > maxSizeAbs dc
            then e
            else start e' (n + 1)

basicRawReduceOnce :: Expr -> Expr
basicRawReduceOnce v@(Var _) = v
basicRawReduceOnce n@(Name _) = n
basicRawReduceOnce a@(Abstr _ _) = if tryEta dc then eta a else a
basicRawReduceOnce a@(Appl (Abstr _ _) _) = beta a
basicRawReduceOnce a@(Appl e f) = basicRawReduceOnce e `Appl` basicRawReduceOnce f

alpha :: Expr -> Expr
alpha (Abstr v e) = Abstr v' (rawBeta v (Var v') e)
  where
    v' = nextValid v
      where
        nextValid v
          | Var (next v) `unboundIn` e = nextValid $ next v
          | otherwise = next v

        next v
          | v == 'z' = 'a'
          | otherwise = chr $ ord v + 1

beta :: Expr -> Expr
beta (Appl (Abstr v e) ve) = rawBeta v ve e

rawBeta :: Char -> Expr -> Expr -> Expr
rawBeta f t e@(Var c)
  | c == f = t
  | otherwise = e
rawBeta f t e@(Appl a b) = Appl (rawBeta f t a) (rawBeta f t b)
rawBeta f t e@(Name n) = e
rawBeta f t e@(Abstr v e')
  | v == f || t == Var v = rawBeta f t $ alpha e
  | otherwise = Abstr v (rawBeta f t e')

eta :: Expr -> Expr
eta x@(Abstr v (Appl e (Var v')))
  | v == v' && Var v `noUnboundIn` e = e
  | otherwise = x

unboundIn :: Expr -> Expr -> Bool
unboundIn a e | a == e = True
unboundIn a (Abstr v e)
  | a == Var v = False
  | otherwise = unboundIn a e
unboundIn a (Appl e f) = unboundIn a e || unboundIn a f
unboundIn _ _ = False

infixr 9 ...
(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)

noUnboundIn :: Expr -> Expr -> Bool
noUnboundIn = not ... unboundIn

size :: Expr -> Int
size (Var _) = 1
size (Name _) = 1
size (Appl e f) = 1 + size e + size f
size (Abstr v e) = 1 + size e

exactEq :: Expr -> Expr -> Bool
exactEq = (==)

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var a) (Var b) = a == b
alphaEq (Name a) (Name b) = a == b
alphaEq (Appl a b) (Appl c d) = alphaEq a c && alphaEq b d
alphaEq (Abstr v e) (Abstr v' e')
  | v == v' = alphaEq e e'
  | otherwise = alphaEq e (rawBeta v (Var v') e')