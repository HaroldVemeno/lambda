module Main where

import Control.Monad
import Data.Map
import Parse
import Print
import Reduce
import Test
import Types

main :: IO ()
main = undefined

repl :: IO ()
repl = repl' defaultContext
  where
    repl' c =
      do
        putStr "\n\\> "
        i <- getLine
        let (o, c') = replDo (i, c)
        putStrLn o
        if quit c' then return () else repl' c'

    replDo :: (String, Context) -> (String, Context)
    replDo (i, c) = doStmt c $ parseStmt "repl" i

    doStmt :: Context -> Result Statement -> (String, Context)
    doStmt c (Left err) = (show err, c)
    doStmt c (Right (Expr e)) = (,) (showExpr $ reduce c e) c
    doStmt c (Right (Command k)) = case k of
      CommandQuit -> (,) "Quitting" c {quit = True}
      CommandLet n e ->
        let r = reduce c e
         in case r of
              Left err -> (show err, c)
              Right e' -> ("Bound", c {names = insert n e' (names c)})
      CommandTree e -> (rawShowTree e, c)
      CommandLoad f -> undefined

test :: Result Bool
test = testParse

quickParse :: IO ()
quickParse = printExpr . parseExpr "input" =<< getLine

quickReduce :: IO ()
quickReduce = printExpr . (basicReduce <=< parseExpr "input") =<< getLine