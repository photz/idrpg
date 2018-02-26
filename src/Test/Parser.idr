module Test.Parser

import Syntax
import Parser
import Token
import Tokenizer

%access export

tests : List (String, Expr)
tests =
  [ ("b", Identifier "b")
  , ("+b", UnaryPlus (Identifier "b"))
  , ("--foo", UnaryMinus (UnaryMinus (Identifier "foo")))
  , ("(a)", Identifier "a")
  , ("((boom))", Identifier "boom")
  , ("(((bar)))", Identifier "bar")
  , ("+-players", UnaryPlus (UnaryMinus (Identifier "players")))
  , ("a+b", BinOp (Identifier "a") Plus (Identifier "b"))
  , ("(a+b)+c",
      BinOp (BinOp (Identifier "a") Plus (Identifier "b"))
            Plus
            (Identifier "c"))
  , ("-(-foo)",
      UnaryMinus $ UnaryMinus $ Identifier "foo")
  , ("a?b:c",
      CondExpr (Identifier "a")
               (Identifier "b")
               (Identifier "c"))
  , ("(a+b)?foo:bar",
      CondExpr (BinOp (Identifier "a") Plus (Identifier "b"))
               (Identifier "foo")
               (Identifier "bar"))
  , ("foo*bar",
      BinOp (Identifier "foo")
            Mul
            (Identifier "bar"))
  , ("foo()", FunCall "foo" [])
  , ("f()+g()", BinOp (FunCall "f" []) Plus (FunCall "g" []))
  , ("f(a)", FunCall "f" [Identifier "a"])
  , ("f(a,b)", FunCall "f" [Identifier "a", Identifier "b"])
  , ("f(a+b,c*d)",
      FunCall "f" [ (BinOp (Identifier "a") Plus (Identifier "b"))
                  , (BinOp (Identifier "c") Mul (Identifier "d"))
                  ])
  , ("123", IntLiteral 123)
  , ("100+200", BinOp (IntLiteral 100) Plus (IntLiteral 200))
  , ("h(1*2)", FunCall "h" [BinOp (IntLiteral 1) Mul (IntLiteral 2)])
  , ("true", BoolLiteral True)
  , ("false", BoolLiteral False)
  , ("true==true", BinOp (BoolLiteral True) Equal (BoolLiteral True))
  ]

test : (String, Expr) -> Either String Expr
test (source, expected) = do
  allTokens <- Right $ Tokenizer.tokens source
  (expr, rest) <- Parser.parseExpr allTokens
  res <- case expr == expected of
              True => Right expected
              False => Left $ "wrong: " ++ (show expr) ++ " \n" ++ (show allTokens)
  pure res


runTests : IO ()
runTests = putStrLn $ show $ map test tests


stmts : List (String, Stmt)
stmts = [ ("let a = b;", VarDecl "a" (Identifier "b"))
        , ("let foo=10+20;",
          VarDecl "foo" (BinOp (IntLiteral 10) Plus (IntLiteral 20)))
        , ("foo = bar;",
          Assignment "foo" (Identifier "bar"))
        , ("x = f(a);",
          Assignment "x" (FunCall "f" [Identifier "a"]))
        , ("if true {} else {}",
          IfStmt (BoolLiteral True) [] [])
        , ("if false {a=a;} else {b=b;}",
          IfStmt (BoolLiteral True)
            [Assignment "a" (Identifier "a")]
            [Assignment "b" (Identifier "b")])
        , ("if 1 { if 1 { return 1; } }",
          IfStmt (IntLiteral 1) 
            [IfStmt (IntLiteral 1)
              [Return (IntLiteral 1)]
              []
            ]
            [])
        ]

stmtTest : (String, Stmt) -> Either String String
stmtTest (source, expected) = do
  tokens <- Right $ Tokenizer.tokens source
  (stmt, _) <- Parser.parseStmt tokens
  case stmt == expected of
    False => Left ("got wrong: ")
    True =>
      Right "passed"
      

stmtTests : IO ()
stmtTests = putStrLn $ show $ map stmtTest stmts

funDefs : List (String, FunDef)
funDefs = [ ("fn f() {}",
             MkFunDef "f" [] [])
          , ("fn g(a) {}",
            MkFunDef "g" ["a"] [])
          , ("fn h(a, b) {}",
            MkFunDef "h" ["a", "b"] [])
          , ("fn i() {let foo = bar;}",
            MkFunDef "i" [] [VarDecl "foo" (Identifier "bar")])
          , ("fn j(a) {a = a;b=b;}",
            MkFunDef "j" ["a"] [ Assignment "a" (Identifier "a")
                               , Assignment "b" (Identifier "b") ])
          , ("fn k() {return 123;}",
            MkFunDef "k" [] [ Return (IntLiteral 123) ])
          , ("fn l() {foo();bar();}",
            MkFunDef "l" [] [ ExprStmt (FunCall "foo" [])
                            , ExprStmt (FunCall "bar" []) ])
          ]
          
funDefTest : (String, FunDef) -> Either String String
funDefTest (src, expFunDef) = do
  tokens <- Right $ Tokenizer.tokens src
  (funDef, _) <- parseFunDef tokens
  case funDef == expFunDef of
    False =>
      Left ("wrong: " ++ (show funDef))
    True =>
      Right "passed"
      
funDefTests : IO ()
funDefTests = putStrLn $ show $ map funDefTest funDefs      
