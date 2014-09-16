
module Main where

import Language.Java.Lexer
import Language.Java.Parser

main :: IO ()
main = interact (show . runTokenizer)
