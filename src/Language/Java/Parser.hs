
module Language.Java.Parser where

import Text.Parsec
import Language.Java.Lexer
import Language.Java.Parser.Internal

parseProgram = parseJava javaProgram
parseTokens = parse javaProgram ""
parseJava p s =
        let s' = runTokenizer s
        in case parse p "" s' of
               Right r -> r
               Left l -> error (show l)
