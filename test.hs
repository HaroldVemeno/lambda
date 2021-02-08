module Test where

import Control.Applicative
import Parse
import Print
import Reduce
import Types

--          name,   input,  expected
tests1 :: [(String, String, Expr)]
tests1 =
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
    )
  ]

tests2 :: [(String, String, Expr, Expr)]
tests2 =
  [ ( "Random 1",
      "(\\ab.b\\ca.dacb)\\po.gho",
      Appl (Abstr 'a' (Abstr 'b' (Appl (Var 'b') (Abstr 'c' (Abstr 'a' (Appl (Appl (Appl (Var 'd') (Var 'a')) (Var 'c')) (Var 'b'))))))) (Abstr 'p' (Abstr 'o' (Appl (Appl (Var 'g') (Var 'h')) (Var 'o')))),
      Abstr 'b' (Appl (Var 'b') (Abstr 'c' (Abstr 'a' (Appl (Appl (Appl (Var 'd') (Var 'a')) (Var 'c')) (Var 'b')))))
    ),
    ( "Church encoding for 5",
      "\\fx.f(f(f(f(fx))))",
      Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x'))))))),
      Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x')))))))
    ),
    ( "5 duplications",
      "(\\fx.f(f(f(f(fx)))))(\\a.aa)p",
      Appl ( Appl (Abstr 'f' (Abstr 'x' (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Appl (Var 'f') (Var 'x')))))))) (Abstr 'a' (Appl (Var 'a') (Var 'a')))) (Var 'p'),
      undefined
    )
  ]

testParse :: Result Bool
testParse =
  doTest
    "Unbound varialble test"
    "a"
    (Var 'a')
    &&& doTest
      "Application test"
      "bc"
      (Appl (Var 'b') (Var 'c'))
    &&& doTest
      "Abstraction test"
      "\\p.p"
      (Abstr 'p' (Var 'p'))
    &&& doTest
      "Abstraction test 2"
      "\\pqr.p"
      (Abstr 'p' (Abstr 'q' (Abstr 'r' (Var 'p'))))
    &&& doTest
      "Parenthesis test"
      "((h)((p)))"
      (Appl (Var 'h') (Var 'p'))
    &&& doTest
      "Unbound varialble whitespace test"
      " a "
      (Var 'a')
    &&& doTest
      "Application whitespace test"
      " b c "
      (Appl (Var 'b') (Var 'c'))
    &&& doTest
      "Abstrastion whitespace test"
      " \\ p  . p "
      (Abstr 'p' (Var 'p'))
    &&& doTest
      "Parenthesis whitespace test"
      "( (  h ) (  ( p   ) )  ) "
      (Appl (Var 'h') (Var 'p'))
    &&& doTest
      "mix test 1"
      "(\\ab.b\\ca.dacb)\\po.gho"
      ( Appl
          ( Abstr
              'a'
              ( Abstr
                  'b'
                  ( Appl
                      (Var 'b')
                      ( Abstr
                          'c'
                          ( Abstr
                              'a'
                              ( Appl
                                  ( Appl
                                      ( Appl
                                          (Var 'd')
                                          (Var 'a')
                                      )
                                      (Var 'c')
                                  )
                                  (Var 'b')
                              )
                          )
                      )
                  )
              )
          )
          ( Abstr
              'p'
              ( Abstr
                  'o'
                  ( Appl
                      ( Appl
                          (Var 'g')
                          (Var 'h')
                      )
                      (Var 'o')
                  )
              )
          )
      )
    &&& doTestC
      "quit command test"
      "quit"
      CommandQuit
    &&& doTestC
      "let command test"
      "let 5 \\fx.f(f(f(f(fx))))"
      ( CommandLet
          "5"
          ( Abstr
              'f'
              ( Abstr
                  'x'
                  ( Appl
                      (Var 'f')
                      ( Appl
                          (Var 'f')
                          ( Appl
                              (Var 'f')
                              ( Appl
                                  (Var 'f')
                                  ( Appl
                                      (Var 'f')
                                      (Var 'x')
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
    &&& doTestC
      "load command test"
      "load C:\\stuff\\haskell\\lambda.txt"
      (CommandLoad " C:\\stuff\\haskell\\lambda.txt")
  where

    testEq :: Statement -> Statement -> Bool
    testEq (Command a) (Command b) = a == b
    testEq (Expr a) (Expr b) = alphaEq a b
    testEq _ _ = False

    doTest c str r = doTestS c str (Expr r)
    doTestC c str r = doTestS c str (Command r)
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
    infixl 2 &&&
    (&&&) = liftA2 (&&)
