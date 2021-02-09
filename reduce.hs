{-# LANGUAGE MultiWayIf #-}

module Reduce where

import Data.Char (chr, ord)
import Types

dc :: Context
dc = defaultContext

basicReduce :: Expr -> Result Expr
basicReduce = reduce dc

reduce :: Context -> Expr -> Result Expr
reduce c e0 = start e0 0
  where
    l0 = size e0
    start e n =
      let e' = iterate basicRawReduceOnce e !! reduceStepSize c
          s = size e'
       in if
              | e == e' -> Right e
              | n >= maxReductions c
                  || s > (maxSizeRel c * l0)
                  || s > maxSizeAbs c ->
                Left $ ReduceError "Doesn't halt" e0
              | otherwise -> start e' (n + 1)

basicRawReduceOnce :: Expr -> Expr
basicRawReduceOnce = rawReduceOnce dc

rawReduceOnce :: Context -> Expr -> Expr
rawReduceOnce c = fn
  where
    fn v@(Var _) = v
    fn n@(Name _) = n
    fn a@(Abstr _ (Appl _ (Var _))) = if tryEta c then eta a else a
    fn a@(Abstr v e) = if tryEager c then Abstr v $ tryReduceOnce c e else a
    fn a@(Appl (Abstr _ _) _) = beta a
    fn a@(Appl e f) = fn e `Appl` fn f

tryReduceOnce :: Context -> Expr -> Expr
tryReduceOnce c e =
  let e' = rawReduceOnce c e
      l = size e
      l' = size e'
   in if l < l' then e else e'

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