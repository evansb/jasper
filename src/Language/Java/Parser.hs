
module Language.Java.Parser where

import Text.Parsec
import Language.Java.Parser.Internal
import Language.Java.Lexer

parseJava :: JParser a -> String -> a
parseJava p s =
        let s' = runTokenizer s
        in case parse p "" s' of
               Right r -> r
               Left l -> error (show l)
