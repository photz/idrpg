module Test.Parser

import Syntax
import Parser
import Token
import Tokenizer

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
  ]

test : (String, Expr) -> Either String Expr
test (source, expected) = do
  allTokens <- Right $ Tokenizer.tokens source
  (expr, rest) <- Parser.parseExpr allTokens
  res <- case expr == expected of
              True => Right expected
              False => Left $ "wrong: " ++ (show expr) ++ " \n" ++ (show allTokens)
  pure res


runTestsx : IO ()
runTestsx = putStrLn (show $ map test tests)

public export
runTests : IO ()
runTests = putStrLn "yo what is UP"

main : IO ()
main = putStrLn "main"
