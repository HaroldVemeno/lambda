{-# LANGUAGE LambdaCase #-}

module Misc where

import qualified Data.Char as CH (chr, ord)
import qualified Data.List as L
import qualified Data.Map as M
import Types

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

getUnboundsIn :: Expr -> [Char]
getUnboundsIn (Name _) = []
getUnboundsIn (Var v) = [v]
getUnboundsIn (Appl a b) = L.nub $ getUnboundsIn a ++ getUnboundsIn b
getUnboundsIn (Abstr v e) = L.delete v $ getUnboundsIn e

isCombinator :: Expr -> Bool
isCombinator = yee []
  where
    yee s (Var v) = v `elem` s
    yee s (Name n) = True
    yee s (Appl a b) = yee s a && yee s b
    yee s (Abstr v e) = yee (v : s) e

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
  | otherwise = alphaEq e (rawBeta v' (Var v) e')
alphaEq _ _ = False

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
          | otherwise = CH.chr $ CH.ord v + 1

beta :: Expr -> Expr
beta (Appl (Abstr v e) ve) = rawBeta v ve e

rawBeta :: Char -> Expr -> Expr -> Expr
rawBeta f t e@(Var c)
  | c == f = t
  | otherwise = e
rawBeta f t e@(Appl a b) = Appl (rawBeta f t a) (rawBeta f t b)
rawBeta f t e@(Name n) = e
rawBeta f t e@(Abstr v e')
  | v == f || v `elem` getUnboundsIn t = rawBeta f t $ alpha e
  | otherwise = Abstr v (rawBeta f t e')

eta :: Expr -> Expr
eta x@(Abstr v (Appl e (Var v')))
  | v == v' && Var v `noUnboundIn` e = e
  | otherwise = x

skify :: Context -> Expr -> Expr
skify c e = sk e
  where
    sk e = case e of
      Var v -> e
      Name n | forceNames c && n `notElem` ["S", "K", "I"] && n `M.member` names c -> sk $ names c M.! n
      Name n -> e
      Appl a b -> Appl (sk a) (sk b)
      Abstr v f | Var v `noUnboundIn` f -> Appl (Name "K") (sk f)
      Abstr v (Appl f (Var v')) | tryEta c && v == v' && Var v `noUnboundIn` f -> sk $ eta e
      Abstr v (Var v') | v == v' -> Name "I"
      Abstr v f@Abstr {} -> sk . Abstr v $ sk f
      Abstr v (Appl a b) -> Appl (Appl (Name "S") (sk $ Abstr v a)) (sk $ Abstr v b)