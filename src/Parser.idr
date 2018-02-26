module Parser

import Syntax
import Token
import Data.String

mutual
  public export
  parseExpr : List (TokenType, String)
            -> Either String (Expr, List (TokenType, String))
  parseExpr Nil = Left "unexpected end"
  parseExpr xxs@((x, theLemma)::xs) = 
    case getParser x prefixParsers of
      Nothing => Left "no prefix parser"
      Just parser => 
        case parser xxs of
          Left e => Left e
          Right (lExpr, restTokens) =>
            parseInfix lExpr restTokens

    where
              getParser
                : TokenType -> List (TokenType, b) -> Maybe b
              getParser x ys = do
                (tokTyp, prefParser) <- find (\(thisTokType, _) => thisTokType == x) ys
                pure prefParser

              prefixParsers
                : List (TokenType, List (TokenType, String) -> Either String (Expr, List (TokenType, String)))
              prefixParsers = [ ( Token.Plus, parsePrefixPlus )
                              , ( Token.Minus, parsePrefixMinus )
                              , ( Token.Identifier, parsePrefixIdent )
                              , ( Token.LeftPar, parsePrefixGroup )
                              , ( Token.Number, parsePrefixNumber )
                              , ( Token.True, parsePrefixBool )
                              , ( Token.False, parsePrefixBool )
                              ]

              infixParsers
                : List (TokenType, Expr -> List (TokenType, String) -> Either String (Expr, List (TokenType, String)))
              infixParsers = [ ( Token.Plus, parseInfixPlus )
                             , ( Token.QuestionMark, parseInfixCond )
                             , ( Token.Mul, parseInfixMul )
                             , ( Token.LeftPar, parseInfixFunCall )
                             , ( Token.Equal, parseInfixEqual )
                             ]

              parseInfix
                : Expr
                -> List (TokenType, String)
                -> Either String (Expr, List (TokenType, String))
              parseInfix lExpr xxs@((tokType, _)::xs) =
                case getParser tokType infixParsers of
                  Nothing =>
                    Right (lExpr, xxs)
                  Just parser =>
                    case parser lExpr xxs of
                      Left e => Left e
                      Right (expr, rest) =>
                        parseInfix expr rest

              parseInfix lExpr Nil = Right (lExpr, Nil)
                              

  parsePrefixBool : List (TokenType, String)
                  -> Either String (Expr, List (TokenType, String))
  parsePrefixBool ((Token.True, _)::xs) =
    Right (BoolLiteral True, xs)
  parsePrefixBool ((Token.False, _)::xs) =
    Right (BoolLiteral False, xs)
  parsePrefixBool ((x, _)::_) =
    Left ("expecting either true or false, got " ++ (show x))

  parsePrefixIdent : List (TokenType, String)
                   -> Either String (Expr, List (TokenType, String))
  parsePrefixIdent ((Token.Identifier, id)::xs) = do
    Right (Identifier id, xs)
  parsePrefixIdent somethingElse = Left "expected an identifier"

  parsePrefixPlus : List (TokenType, String)
                  -> Either String (Expr, List (TokenType, String))
  parsePrefixPlus (x::xs) = do
    (expr, rest) <- parseExpr xs
    pure (UnaryPlus expr, rest)
  

  parsePrefixMinus : List (TokenType, String)
                   -> Either String (Expr, List (TokenType, String))
  parsePrefixMinus (x::xs) = do
    (expr, rest) <- parseExpr xs
    pure (UnaryMinus expr, rest)

  parsePrefixGroup : List (TokenType, String)
                   -> Either String (Expr, List (TokenType, String))
  parsePrefixGroup ((Token.LeftPar, _)::xs) = do
    (expr, (Token.RightPar, _)::rest) <- parseExpr xs
      | _ => Left "expecting a right parenthesis"
    pure (expr, rest)

  parsePrefixNumber : List (TokenType, String)
                    -> Either String (Expr, List (TokenType, String))
  parsePrefixNumber ((Token.Number, numRaw)::xs) = do
    val <- case parsePositive {a=Int} numRaw of
                 Nothing => Left "not a valid number"
                 Just v => Right v
    pure (IntLiteral val, xs)

  parseInfixMul : Expr
                -> List (TokenType, String)
                -> Either String (Expr, List (TokenType, String))
  parseInfixMul lExpr ((TokenType.Mul, _)::xs) = do
    (rExpr, rest) <- parseExpr xs
    pure (BinOp lExpr Mul rExpr, rest)
  parseInfixMul _ _ = Left "expected a *"

  parseInfixPlus : Expr
                 -> List (TokenType, String)
                 -> Either String (Expr, List (TokenType, String))
  parseInfixPlus lExpr ((Token.Plus, _)::xs) = do
    (rExpr, rest) <- parseExpr xs
    pure (BinOp lExpr Plus rExpr, rest)

  parseInfixCond : Expr
                 -> List (TokenType, String)
                 -> Either String (Expr, List (TokenType, String))
  parseInfixCond lExpr ((Token.QuestionMark, _)::xs) = do
    (ifExpr, (Token.Colon, _)::tokensAfterColon) <- parseExpr xs
      | _ => Left "expecting a colon"
    (elseExpr, afterElse) <- parseExpr tokensAfterColon
    pure (CondExpr lExpr ifExpr elseExpr, afterElse)

  parseInfixCond a b = Left "expected a question mark"

  parseInfixEqual : Expr
                  -> List (TokenType, String)
                  -> Either String (Expr, List (TokenType, String))
  parseInfixEqual lExpr ((Equal, _)::xs) = do
    (rExpr, rest) <- parseExpr xs
    pure (BinOp lExpr Equal rExpr, rest)
  

  parseArgList
    : List (TokenType, String)
    -> Either String (List Expr, List (TokenType, String))
  parseArgList xs@((Token.RightPar, _)::_) = Right ([], xs)
  parseArgList xs = do
               (firstArg, rest) <- parseExpr xs
               (args, rest) <- parseList rest Nil
               pure (firstArg::args, rest)
    where
      parseList : List (TokenType, String)
                -> List Expr
                -> Either String (List Expr, List (TokenType, String))
      parseList ((Token.Comma, _)::xs) args = do
        (nextArg, rest) <- parseExpr xs
        parseList rest (nextArg::args)
      parseList xs@((Token.RightPar, _)::_) args =
        Right (reverse args, xs)
      parseList ((x, _)::_) _ =
        Left ("unexpected token " ++ (show x))

  parseInfixFunCall : Expr
                    -> List (TokenType, String)
                    -> Either String (Expr, List (TokenType, String))
  parseInfixFunCall (Identifier id) ((Token.LeftPar, _)::xs) = do
    (args, (Token.RightPar, _)::rest) <- parseArgList xs
      | _ => Left "expected a right parenthesis"
    pure (FunCall id args, rest)
    
  parseInfixFunCall _ _ = Left "invalid function call"


  public export
  
  parseStmt : List (TokenType, String)
            -> Either String (Stmt, List (TokenType, String))
  parseStmt ((Let, _)::(Identifier, id)::(Assign, _)::xs) = do
    (expr, (Semi, _)::afterSemi) <- parseExpr xs
      | _ => Left "expected a semicolon at the end of statement"
    pure (VarDecl id expr, afterSemi)

  parseStmt ((Identifier, id)::(Assign, _)::xs) = do
    (expr, (Semi, _)::afterSemi) <- parseExpr xs
      | _ => Left "expected a semicolon at the end of statement"
    pure (Assignment id expr, afterSemi)

  parseStmt ((Return, _)::xs) = do
    (expr, (Semi, _)::afterSemi) <- parseExpr xs
      | Left "expected a semicolon after statement"
    pure (Return expr, afterSemi)

  parseStmt xs@((Identifier, id)::_) = do
    (expr, (Semi, _)::afterSemi) <- parseExpr xs
      | Left "expected a semicolon after statement"
    pure (ExprStmt expr, afterSemi)

  parseStmt ((If, _)::xs) = do
    (cond, (LeftBrace, _)::xs) <- parseExpr xs
      | _ => Left "expecting { after condition in head of if statement"
    (stmts, (RightBrace, _)::xs) <- parseStmtList xs
      | (_, (_, lemma)::_) => 
        Left ("expecting a } after statement list in body of if, got " ++ lemma)
  
    case xs of
      (Else, _)::(LeftBrace, _)::rest => do
        (elseStmts, (RightBrace, _)::afterStmts) <- parseStmtList rest
          | _ => Left "expected }"
        Right (IfStmt cond stmts elseStmts, afterStmts)
      _ =>
        Right (IfStmt cond stmts [], xs)

  


  parseStmt ((_, lemma)::_) =
    Left "invalid statement"

  public export

  parseStmtList : List (TokenType, String)
                -> Either String (List Stmt, List (TokenType, String))
  parseStmtList xs = parseList xs Nil
    where
      inFirst : TokenType -> Bool
      inFirst Identifier = True
      inFirst Let = True
      inFirst Return = True
      inFirst If = True
      inFirst _ = False

      parseList : List (TokenType, String)
                -> List Stmt
                -> Either String (List Stmt, List (TokenType, String))
      parseList Nil ys = Right (reverse ys, Nil)
      parseList ((x, xlemma)::xs) ys with (inFirst x) 
        | True = do
          (stmt, rest) <- parseStmt ((x, xlemma)::xs)
          parseList rest (stmt::ys)

        | False = Right (reverse ys, ((x, xlemma)::xs))



public export

parseParamList : List (TokenType, String)
               -> Either String (List String, List (TokenType, String))
parseParamList xs@((RightPar, _)::_) = Right ([], xs)
parseParamList ((Identifier, firstParam)::xs) = do
               (otherParams, afterParams) <- parseList xs Nil
               pure (firstParam::otherParams, afterParams)
  where
    parseList : List (TokenType, String)
              -> List String
              -> Either String (List String, List (TokenType, String))
    parseList ((Comma, _)::(Identifier, id)::xs) params =
      parseList xs (id::params)

    parseList xs params =
      Right (params, xs)




public export

parseFunDef : List (TokenType, String)
            -> Either String (FunDef, List (TokenType, String))
parseFunDef ((Fn, _)::(Identifier, id)::(LeftPar, _)::xs) = do
  (params, (RightPar, _)::(LeftBrace, _)::rest) <- parseParamList xs
    | _ => Left "error while parsing head of function definition"
  (stmts, (RightBrace, _)::afterFunDef) <- parseStmtList rest
    | _ => Left "expected }"
  pure (MkFunDef id params stmts, afterFunDef)

parseFunDef _ = Left "invalid function definition"
