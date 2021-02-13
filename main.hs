module Main where

import Control.Monad
import Data.Map (insert)
import Debug.Trace
import Parse
import Print
import Reduce
import System.IO
import Test
import Types

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
          let r = reduce c e
           in case r of
                Left err -> (show err, c)
                Right e' -> ("Bound", c {names = insert n e' (names c)})
      CommandTree e -> return (rawShowTree e, c)
      CommandLoad f -> do
        file <- readFile f
        putStrLn $ "The file :\n" ++ file
        let p = parseFile f file
        case p of
          Left err -> return (show err, c)
          Right a -> do
            c' <- foldl (\c f -> (\c -> snd <$> doStmt c (Right f)) =<< c) (return c) a
            print $ names c'
            return ("", c')
      CommandStep e -> stepReduce c e >> return ("", c)

test :: Result Bool
test = testParse

quickParse :: IO ()
quickParse = printExpr . parseExpr "input" =<< getLine

quickReduce :: IO ()
quickReduce = printExpr . (basicReduce <=< parseExpr "input") =<< getLine