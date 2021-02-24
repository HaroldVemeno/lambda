module Test where

import Control.Applicative
import Parse
import Print
import Reduce
import Types
import Misc

parseTests ::
  [ ( String, -- Name
      String, -- Input
      Expr -- Parse output
    )
  ]
parseTests =
  [ ( "Var test",
      "a",
      Var 'a'
    ),
    ( "Appl test",
      "bc",
      Appl (Var 'b') (Var 'c')
    ),
    ( "Abstr test",
      "\\p.p",
      Abstr 'p' (Var 'p')
    ),
    ( "Abstr test 2 - chain",
      "\\pqr.p",
      Abstr 'p' (Abstr 'q' (Abstr 'r' (Var 'p')))
    ),
    ( "Parens test",
      "((h)((p)))",
      Appl (Var 'h') (Var 'p')
    ),
    ( "Var whitespace test",
      " a ",
      Var 'a'
    ),
    ( "Appl whitespace test",
      " b c ",
      Appl (Var 'b') (Var 'c')
    ),
    ( "Abstr whitespace test",
      " \\ p  . p ",
      Abstr 'p' (Var 'p')
    ),
    ( "Parens whitespace test",
      "( (  h ) (  ( p   ) )  ) ",
      Appl (Var 'h') (Var 'p')
    ),
    ( "Random 1",
      "(\\ab.b\\ca.dacb)\\po.gho",
      Appl (Abstr 'a' (Abstr 'b' (Appl (Var 'b') (Abstr 'c' (Abstr 'a' (Appl (Appl (Appl (Var 'd') (Var 'a')) (Var 'c')) (Var 'b'))))))) (Abstr 'p' (Abstr 'o' (Appl (Appl (Var 'g') (Var 'h')) (Var 'o'))))
    ),
    ( "Church encoding for 5",
      "\\fx.f(f(f(f(fx))))",
      Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x')))))))
    ),
    ( "5 duplications",
      "(\\fx.f(f(f(f(fx)))))(\\a.aa)p",
      Appl (Appl (Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x')))))))) (Abstr 'a' (Appl (Var 'a') (Var 'a')))) (Var 'p')
    )
  ]

reduceTests :: [(String, String, String)]
reduceTests =
  [ ( "9 / 3 = 3",
      "(\\n.((\\f.(\\x.x x) (\\x.f (x x))) (\\cnmfx.(\\d.(\\n.n (\\x.(\\ab.b)) (\\ab.a)) d ((\\fx.x) f x) (f (c d m f x)))"
        ++ "((\\mn.n (\\nfx.n (\\gh.h (g f)) (\\u.x) (\\u.u)) m) n m))) ((\\nfx. f (n f x)) n))"
        ++ "(\\fx.f (f (f (f (f (f (f (f (f x))))))))) (\\fx.f (f (f x)))",
      "\\fx.f(f(fx))"
    ),
    ( "Random 1",
      "(\\ab.b\\ca.dacb)\\po.gho",
      "\\b.b(\\ca.dacb)"
    ),
    (
      "unbound checking test",
      "(\\fx.f(fx))(\\fx.f(fx))",
      "\\fx.f(f(f(fx)))"
    )
  ]

commandTests :: [(String, String, Command)]
commandTests =
  [ ( "Quit command test",
      "Quit",
      CommandQuit
    ),
    ( "Let command test",
      "Let 5 \\fx.f(f(f\n(f(fx))))",
      CommandLet
        "5"
        (Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x'))))))))
    ),
    ( "Load command test",
      "Load C:\\stuff\\haskell\\lambda.txt",
      CommandLoad "C:\\stuff\\haskell\\lambda.txt"
    )
  ]

testParse :: Result Bool
testParse =
  foldl1 (&&&) (uncurry3 doTestE <$> parseTests)
    &&& foldl1 (&&&) (uncurry3 doTestR <$> reduceTests)
    &&& foldl1 (&&&) (uncurry3 doTestC <$> commandTests)

testEq :: Statement -> Statement -> Bool
testEq (Command a) (Command b) = a == b
testEq (Expr a) (Expr b) = alphaEq a b
testEq _ _ = False

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

doTestE :: [Char] -> [Char] -> Expr -> Result Bool
doTestE c str r = doTestS c str (Expr r)

doTestC :: [Char] -> [Char] -> Command -> Result Bool
doTestC c str r = doTestS c str (Command r)

doTestS :: [Char] -> [Char] -> Statement -> Result Bool
doTestS c str r =
  let par = parseStmt c str
      same = testEq r <$> par
      un (Right s) = s
   in if same == Right False
        then
          Left $
            TestError
              ( "Failed " ++ c
                  ++ "\nInput: "
                  ++ str
                  ++ "\nOutput: "
                  ++ show (un par)
                  ++ "\nExpected: "
                  ++ show r
              )
        else same

doTestR :: [Char] -> [Char] -> [Char] -> Result Bool
doTestR c f t =
  let pf = parseExpr (c ++ " (from)") f
      pt = parseExpr (c ++ " (to)") t
      same = liftA2 alphaEq (reduce defaultContext =<< pf) pt
      un (Right s) = s
   in if same == Right False
        then
          Left $
            TestError
              ( "Failed " ++ c
                  ++ "\nInput: "
                  ++ f
                  ++ "\nOutput: "
                  ++ showExpr pt
                  ++ "\nExpected: "
                  ++ t
              )
        else same

infixl 2 &&&

(&&&) :: Result Bool -> Result Bool -> Result Bool
(&&&) = liftA2 (&&)
