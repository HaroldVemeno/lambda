module Print (showExpr, printExpr) where

import Control.Monad
import Data.List
import Types

showExpr :: Result Expr -> Result String
showExpr = fmap rawShowExpr

printExpr :: Result Expr -> IO ()
printExpr (Left err) = print err
printExpr (Right ex) = putStrLn $ rawShowExpr ex

rawShowExpr :: Expr -> String
rawShowExpr (Var a) = a : ""
rawShowExpr (Name s) = " " ++ s ++ " "
rawShowExpr (Abstr a e@(Abstr _ _)) = '\\' : a : tail (rawShowExpr e)
rawShowExpr (Abstr a e) = '\\' : a : '.' : rawShowExpr e
rawShowExpr (Appl a@(Abstr _ _) b@(Appl _ _)) = "(" ++ rawShowExpr a ++ ")(" ++ rawShowExpr b ++ ")"
rawShowExpr (Appl a b@(Appl _ _)) = rawShowExpr a ++ "(" ++ rawShowExpr b ++ ")"
rawShowExpr (Appl a@(Abstr _ _) b@(Abstr _ _)) = "(" ++ rawShowExpr a ++ ")" ++ "(" ++ rawShowExpr b ++ ")"
rawShowExpr (Appl a@(Abstr _ _) b) = "(" ++ rawShowExpr a ++ ")" ++ rawShowExpr b
rawShowExpr (Appl a b@(Abstr _ _)) = rawShowExpr a ++ "(" ++ rawShowExpr b ++ ")"
rawShowExpr (Appl a b) = rawShowExpr a ++ rawShowExpr b

simpleRawShowTree :: Expr -> String
simpleRawShowTree = intercalate "\n" . lol
  where
    lol :: Expr -> [String]
    lol (Var c) = ["Var " ++ [c]]
    lol (Name n) = ["Name " ++ n]
    lol (Appl a b) = join [["Appl"], mid $ lol a, end $ lol b]
    lol (Abstr c e) = join [["Abstr " ++ [c]], end $ lol e]

    mid :: [String] -> [String]
    mid a = ("|-" ++ head a) : (("| " ++) <$> tail a)

    end a = ("`-" ++ head a) : (("  " ++) <$> tail a)

{-
>>> rawShowTree $ Appl (Abstr 'a' (Abstr 'b' (Var 'a'))) (Abstr 'b' (Appl (Appl (Var 'a') (Var 'c') ) (Var 'a')))
"Appl
|-Abstr a b
| `-Var a
`-Abstr b
  `-Appl
    |-Var a
    |-Var c
    `-Var a"

-}

rawShowTree :: Expr -> String
rawShowTree = intercalate "\n" . lol
  where
    lol :: Expr -> [String]
    lol (Var c) = ["Var " ++ [c]]
    lol (Name n) = ["Name " ++ n]
    lol (Appl a b) = case a of
      --  ¯\_(ツ)_/¯ lol no
      Appl _ _ -> join [[head $ lol a], takeWhile ((/= "`-") . take 2) . tail . lol $ a, mid . fmap (drop 2) . dropWhile ((/= "`-") . take 2) . tail . lol $ a, end . lol $ b]
      _ -> join [["Appl"], mid $ lol a, end $ lol b]
    lol (Abstr c e) = case e of
      Abstr _ _ -> join [["Abstr " ++ [c] ++ " " ++ drop 6 (head $ lol e)], tail $ lol e]
      _ -> join [["Abstr " ++ [c]], end $ lol e]

    mid :: [String] -> [String]
    mid a = ("|-" ++ head a) : (("| " ++) <$> tail a)

    end :: [String] -> [String]
    end a = ("`-" ++ head a) : (("  " ++) <$> tail a)
