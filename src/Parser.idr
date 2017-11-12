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
                              ]

              infixParsers
                : List (TokenType, Expr -> List (TokenType, String) -> Either String (Expr, List (TokenType, String)))
              infixParsers = [ ( Token.Plus, parseInfixPlus )
                             , ( Token.QuestionMark, parseInfixCond )
                             , ( Token.Mul, parseInfixMul )
                             , ( Token.LeftPar, parseInfixFunCall )
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
