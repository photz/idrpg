module Syntax

%access public export


data Op = Plus | Minus | Mul | Equal


Eq Op where
  Plus == Plus = True
  Minus == Minus = True
  Mul == Mul = True
  Equal == Equal = True
  _ == _ = False


Show Op where
  show Plus = "Plus"
  show Minus = "Minus"
  show Mul = "Mul"
  show Equal = "Equal"

data Expr = UnaryPlus Expr
          | UnaryMinus Expr
          | UnaryNot Expr
          | Identifier String
          | IntLiteral Int
          | BoolLiteral Bool
          | BinOp Expr Op Expr
          | CondExpr Expr Expr Expr
          | FunCall String (List Expr)

Show Expr where
  show (IntLiteral n) = "IntLiteral " ++ (show n)
  show (BoolLiteral val) = "BoolLiteral " ++ (show val)
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
  (BoolLiteral l) == (BoolLiteral r) = l == r
  a == b = False
  

data Stmt = VarDecl String Expr
          | Assignment String Expr
          | Return Expr
          | ExprStmt Expr
          | IfStmt Expr (List Stmt) (List Stmt)

Eq Stmt where
  (VarDecl id1 expr1) == (VarDecl id2 expr2) =
    id1 == id2 && expr1 == expr2
  (Assignment id1 expr1) == (Assignment id2 expr2) =
    id1 == id2 && expr1 == expr2
  (Return l) == (Return r) = l == r
  (ExprStmt l) == (ExprStmt r) = l == r
  (IfStmt expr1 if1 else1) == (IfStmt expr2 if2 else2) = True
  _ == _ = False
  

data FunDef = MkFunDef String (List String) (List Stmt)

Eq FunDef where
  (MkFunDef id1 params1 stmts1) == (MkFunDef id2 params2 stmts2) =
    id1 == id2 && params1 == params2 && stmts1 == stmts2

Show FunDef where
  show (MkFunDef id params stmts) =
    "FunDef " ++ (show params) --++ " " ++ (show stmts)
