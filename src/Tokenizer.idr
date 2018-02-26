module Tokenizer 

import Data.String
import Token

keywords : String -> Maybe Token.TokenType
keywords "def" = Just Def
keywords "else" = Just Else
keywords "false" = Just False
keywords "fn" = Just Fn
keywords "if" = Just If
keywords "let" = Just Let
keywords "return" = Just Return
keywords "true" = Just True
keywords _ = Nothing

myTakeWhile : (Char -> Bool) -> List Char -> (String, List Char)
myTakeWhile p xs = f xs Nil
  where
    f : List Char -> List Char -> (String, List Char)
    f Nil ys = (reverse $ pack ys, Nil)
    f (x::xs) ys with (p x)
      | True  = f xs (x::ys)
      | False = (reverse $ pack ys, x::xs)

data CharType = Alpha | Digit | NonAlphaNum

charType : Char -> CharType
charType x =
  case isAlpha x of
    True => Alpha
    False =>
      case isDigit x of
        True => Digit
        False => NonAlphaNum

public export
tokens : String -> List (TokenType, String)
tokens x = f (unpack x)
  where
    f : List Char -> List (TokenType, String)
    f Nil = []
    f ('+'::xs) = (Plus, "+")::f xs
    f ('-'::xs) = (Minus, "-")::f xs
    f ('*'::xs) = (Mul, "*")::f xs
    f ('('::xs) = (LeftPar, "(")::f xs
    f (')'::xs) = (RightPar, ")")::f xs
    f ('{'::xs) = (LeftBrace, "{")::f xs
    f ('}'::xs) = (RightBrace, "}")::f xs
    f (';'::xs) = (Semi, ";")::f xs
    f ('='::'='::xs) = (Equal, "==")::f xs
    f ('='::xs) = (Assign, "=")::f xs
    f (','::xs) = (Comma, ",")::f xs
    f (':'::xs) = (Colon, ":")::f xs
    f ('?'::xs) = (QuestionMark, "?")::f xs
    f (' ':: xs) = f xs
    f ('\n'::xs) = f xs
    f (x::xs) with (charType x)
      | Alpha = let (lemma, rest) = myTakeWhile isAlpha (x::xs) in
                case keywords lemma of
                  Nothing =>
                    (Identifier, lemma)::f rest
                  Just keyword =>
                    (keyword, lemma)::f rest

      | Digit = let (lemma, rest) = myTakeWhile isDigit (x::xs) in
                case parsePositive {a=Int} lemma of
                  Nothing => f xs
                  Just number =>
                    (Number, lemma)::f rest

      | NonAlphaNum = f xs
