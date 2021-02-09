module Main where

import Control.Monad
import Parse
import Print
import Reduce
import Test
import Types

main :: IO ()
main = undefined

test :: Result Bool
test = testParse

quickParse :: IO ()
quickParse = printExpr . parseExpr "input" =<< getLine

quickReduce :: IO ()
quickReduce = printExpr . (basicReduce <=< parseExpr "input") =<< getLine