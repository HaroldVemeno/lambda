{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Reduce (reduce, basicReduce, tryReduceLet, stepReduce, rawBeta) where

import Data.Char (chr, ord)
import Data.List
import Data.Map (member, (!))
import Debug.Trace
import Print
import Types
import Misc

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
      let itl = iterate (rawLoggedReduceOnce c drs . snd) (drs, e)
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
                let out =
                      intercalate "\n" . zipWith ((++) . show) [n + 1 .. n'] $
                        (": Reduction by " ++) . by <$> filter reduced rss
                putStrLn out
                printExpr (Right e')
                start e' n'

tryReduceLet :: Context -> Expr -> Expr
tryReduceLet c e0 = case reduce
  c {maxReductions = 200, maxSizeAbs = 400, maxSizeRel = 30, forceNames = True}
  e0 of
  Right e -> e
  Left a -> e0

--  tryReduceAppl :: Context -> Expr -> Expr
--  tryReduceAppl c e0 = case reduce
  --  c {maxReductions = 200, maxSizeAbs = 400, maxSizeRel = 30, forceNames = False}
  --  e0 of
  --  Right e -> e
  --  Left a -> e0

basicRawReduceOnce :: Expr -> Expr
basicRawReduceOnce = snd . rawReduceOnce dc drs

-- TODO: check for unbounds in Names
rawLoggedReduceOnce :: Context -> ReduceState -> Expr -> (ReduceState, Expr)
rawLoggedReduceOnce c = fn
  where
    fn s a | reduced s = (s, a)
    fn s v@(Var _) = (s, v)
    fn s m@(Name n)
      | forceNames c && n `member` names c = (s {reduced = True, by = "Forced Name Substitution of " ++ n}, names c ! n)
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

rawReduceOnce :: Context -> ReduceState -> Expr -> (ReduceState, Expr)
rawReduceOnce c = fn
  where
    fn s a | reduced s = (s, a)
    fn s v@(Var _) = (s, v)
    fn s m@(Name n)
      | forceNames c && n `member` names c = (s {reduced = True}, names c ! n)
      | otherwise = (s, m)
    fn s a@(Abstr v i@(Appl e (Var v')))
      | tryEta c =
        if eta a == a
          then
            let (s', i') = fn s i
             in (s', Abstr v i')
          else (s {reduced = True}, eta a)
    fn s a@(Abstr v e) = let (s', e') = fn s e in (s', Abstr v e')
    fn s a@(Appl (Abstr _ _) _) | not $ reduced s = (s {reduced = True}, beta a)
    fn s a@(Appl (Name n) e)
      | n `member` names c = (s {reduced = True}, Appl (names c ! n) e)
    fn s a@(Appl e f) =
      let (s', e') = fn s e
       in if reduced s'
            then (s', Appl e' f)
            else
              let (s'', f') = fn s f
               in (s'', Appl e f')