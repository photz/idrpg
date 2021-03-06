module Main

import Token
import Tokenizer
import Parser
import Syntax
import SymTab

import Data.Vect
import Data.String


foo : Either FileError String -> IO ()
foo (Right s) = do
  putStrLn $ show $ Tokenizer.tokens s
  putStrLn $ show $ Parser.parseExpr $ Tokenizer.tokens s
foo _ = putStrLn "unable to read file"


type : Expr -> Either String Type
type (IntLiteral n) = Right Int
type (BoolLiteral b) = Right Bool


main : IO ()
main = readFile "test.txt" >>= foo


