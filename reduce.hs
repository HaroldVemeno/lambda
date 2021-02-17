{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Reduce (reduce, basicReduce, tryReduce, stepReduce, alphaEq) where

import Data.Char (chr, ord)
import Data.List
import Data.Map (member, (!))
import Debug.Trace
import Print
import Types

dc :: Context
dc = defaultContext

basicReduce :: Expr -> Result Expr
basicReduce = reduce dc

data ReduceState = ReduceState
  { reduced :: Bool,
    by :: String
  }

drs :: ReduceState
drs =
  ReduceState
    { reduced = False,
      by = "Nothing?? how did u get here"
    }

reduce :: Context -> Expr -> Result Expr
reduce c e0 = start e0 0
  where
    l0 = size e0
    start e n =
      let red = iterate (rawReduceOnce c drs . snd) (drs, e)
          (lst, e') = red !! reduceStepSize c
          l = size e'
       in if
              | e == e' || not (reduced lst) -> Right e'
              | n >= maxReductions c
                  || l > (maxSizeRel c * l0)
                  || l > maxSizeAbs c ->
                Left $ ReduceError "Doesn't halt" e0
              | otherwise -> start e' (n + reduceStepSize c)

stepReduce :: Context -> Expr -> IO (Result Expr)
stepReduce c e0 = start e0 0
  where
    l0 = size e0
    start e n =
      let itl = iterate (rawReduceOnce c drs . snd) (drs, e)
          red = take (reduceStepSize c) . takeWhile (\(rs, _) -> reduced rs) . tail $ itl
          rss = fst <$> red
          dn' = length red
          (lst, e') = if null red then head itl else last red
          l = size e'
          n' = n + dn'
       in if
              | e == e' || not (reduced lst) -> do
                putStrLn $ "Reduction finished after " ++ show n' ++ " reductions!"
                printExpr (Right e')
                return $ Right e'
              | n >= maxReductions c
                  || l > (maxSizeRel c * l0)
                  || l > maxSizeAbs c -> do
                putStrLn "Expression doesn't halt in limits and keeps expanding."
                printExpr (Right e')
                return . Left $ ReduceError "No halt in limit" e0
              | otherwise -> do
                let out = intercalate "\n" . zipWith ((++) . show) [n+1..n'] $ (": Reduction by " ++) . by <$> filter reduced rss
                putStrLn out
                printExpr (Right e')
                start e' n'

tryReduce :: Context -> Expr -> Expr
tryReduce c e0 = case reduce c{maxReductions = 100, maxSizeAbs = 400, maxSizeRel = 20, forceNames = True} e0 of
  Right e -> e
  Left  a -> e0

basicRawReduceOnce :: Expr -> Expr
basicRawReduceOnce = snd . rawReduceOnce dc drs

-- TODO: check for unbounds in Names
rawReduceOnce :: Context -> ReduceState -> Expr -> (ReduceState, Expr)
rawReduceOnce c = fn
  where
    fn s a | reduced s = (s, a)
    fn s v@(Var _) = (s, v)
    fn s m@(Name n) | forceNames c && n `member` names c= (s{reduced = True, by = "Forced Name Substitution of " ++ n}, names c ! n)
                    | otherwise = (s, m)
    fn s a@(Abstr v i@(Appl e (Var v')))
      | tryEta c =
        if eta a == a
          then
            let (s', i') = fn s i
             in (s', Abstr v i')
          else (s {reduced = True, by = "Eta"}, eta a)
    fn s a@(Abstr v e) = let (s', e') = fn s e in (s', Abstr v e')
    fn s a@(Appl (Abstr _ _) _) | not $ reduced s = (s {reduced = True, by = "Beta"}, beta a)
    fn s a@(Appl (Name n) e)
      | n `member` names c = (s {reduced = True, by = "Name Substitution of " ++ n}, Appl (names c ! n) e)
    fn s a@(Appl e f) =
      let (s', e') = fn s e
       in if reduced s'
            then (s', Appl e' f)
            else
              let (s'', f') = fn s f
               in (s'', Appl e f')

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
  | v == f || v `elem` getUnboundsIn t = rawBeta f t $ alpha e
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

getUnboundsIn :: Expr -> [Char]
getUnboundsIn (Name _) = []
getUnboundsIn (Var v) = [v]
getUnboundsIn (Appl a b) = nub $ getUnboundsIn a ++ getUnboundsIn b
getUnboundsIn (Abstr v e) = delete v $ getUnboundsIn e

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