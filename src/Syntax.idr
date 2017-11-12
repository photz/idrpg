module Syntax

public export
data Op = Plus | Minus | Mul

public export
Eq Op where
  Plus == Plus = True
  Minus == Minus = True
  Mul == Mul = True
  _ == _ = False

public export
Show Op where
  show Plus = "Plus"
  show Minus = "Minus"
  show Mul = "Mul"

public export
data Expr = UnaryPlus Expr
          | UnaryMinus Expr
          | UnaryNot Expr
          | IntLiteral Int
          | Identifier String
          | BinOp Expr Op Expr
          | CondExpr Expr Expr Expr
          | FunCall String (List Expr)


public export
Show Expr where
  show (IntLiteral n) = "IntLiteral " ++ (show n)
  show (Identifier id) = "Identifier " ++ id
  show (UnaryPlus e) = "UnaryPlus " ++ (show e)
  show (UnaryMinus e) = "UnaryMinus " ++ (show e)
  show (UnaryNot e) = "UnaryNot " ++ (show e)
  show (BinOp l op r) =
    "BinOp " ++ (show l) ++ " " ++ (show op) ++ " " ++ (show r)
  show (CondExpr cond ifExpr elseExpr) =
    "ConditionalExpr " ++ (show ifExpr) ++ (show elseExpr)
  show (FunCall id exprs) =
    "FunCall " ++ id ++ " " ++ (show exprs)

public export
Eq Expr where
  (UnaryPlus l) == (UnaryPlus r) = l == r
  (UnaryMinus l) == (UnaryMinus r) = l == r
  (UnaryNot l) == (UnaryNot r) = l == r
  (IntLiteral l) == (IntLiteral r) = l == r
  (Identifier l) == (Identifier r) = l == r
  (BinOp l1 op1 r1) == (BinOp l2 op2 r2) =
    l1 == l2 && op1 == op2 && r1 == r2
  (FunCall id1 exprs1) == (FunCall id2 exprs2) =
    id1 == id2 -- TODO
  (CondExpr cond1 if1 else1) == (CondExpr cond2 if2 else2) =
    cond1 == cond2 && if1 == if2 && else1 == else2
  a == b = False
