{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Map (insert, lookup, (!))
import Data.Maybe
import Debug.Trace
import Text.Read
import Prelude hiding (lookup)
import Parse
import Print
import Reduce
import System.IO
import Test
import Types
import Misc

main :: IO ()
main = repl

repl :: IO ()
repl = repl' defaultContext
  where
    repl' c =
      do
        putStr "\n\\> "
        hFlush stdout
        i <- getLine
        (o, c') <- replDo (i, c)
        putStrLn o
        if quit c' then return () else repl' c'

    replDo :: (String, Context) -> IO (String, Context)
    replDo (i, c) = doStmt c $ parseStmt "repl" i

    doStmt :: Context -> Result Statement -> IO (String, Context)
    doStmt c (Left err) = return (show err, c)
    doStmt c (Right (Expr e)) = return $ (,) (showExpr $ reduce c e) c
    doStmt c (Right (Command k)) = case k of
      CommandQuit -> return $ (,) "Quitting" c {quit = True}
      CommandLet n e ->
        --redefinition warning?
        return $
          let r = tryReduceLet c e in ("Bound", c {names = insert n r (names c)})
      CommandTree e -> return (rawShowTree e, c)
      CommandLoad f -> do
        file <- readFile f
        --putStrLn $ "The file :\n" ++ file
        let p = parseFile f file
        case p of
          Left err -> return (show err, c)
          Right a -> do
            c' <- foldl (\c f -> (\c -> snd <$> doStmt c (Right (Command f))) =<< c) (return c) a
            --print $ names c'
            return ("", c')
      CommandStep e -> stepReduce c e >> return ("", c)
      CommandShow (Name n) -> return $
        (,c) $ case lookup n (names c) of
          Just e -> rawShowExpr e
          Nothing -> n ++ " was not found."
      CommandShow e -> return (rawShowExpr e, c)
      CommandSet n v ->
        let prp f t = show f ++ " -> " ++ show t
            try v r = let e = readMaybe v in maybe ("Bad value", c) r e
         in return $ case n of
              "maxReductions" -> try v $ \e -> (prp (maxReductions c) e, c {maxReductions = e})
              "reduceStepSize" -> try v $ \e -> (prp (reduceStepSize c) e, c {reduceStepSize = e})
              "maxSizeAbs" -> try v $ \e -> (prp (maxSizeAbs c) e, c {maxSizeAbs = e})
              "maxSizeRel" -> try v $ \e -> (prp (maxSizeRel c) e, c {maxSizeRel = e})
              "tryEta" -> try v $ \e -> (prp (tryEta c) e, c {tryEta = e})
              "forceNames" -> try v $ \e -> (prp (forceNames c) e, c {forceNames = e})
              _ -> ("Unknown setting", c)
      CommandSkify e -> return (rawShowExpr $ skify c e, c) 

test :: Result Bool
test = testParse

quickParse :: IO ()
quickParse = print . parseExpr "input" =<< getLine

quickReduce :: IO ()
quickReduce = printExpr . (basicReduce <=< parseExpr "input") =<< getLine
