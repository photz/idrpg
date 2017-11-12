module Main

import Token
import Tokenizer
import Parser
import Syntax

import Data.Vect
import Data.String

foo : Either FileError String -> IO ()
foo (Right s) = do
  putStrLn $ show $ Tokenizer.tokens s
  putStrLn $ show $ Parser.parseExpr $ Tokenizer.tokens s
foo _ = putStrLn "unable to read file"

main : IO ()
main = readFile "test.txt" >>= foo

