
module Language.Java.Parser where

import Text.Parsec
import qualified Language.Java.Parser.Core as C
import Language.Java.Lexer

type JParser a = C.JParser a

parseJava :: JParser a -> String -> a
parseJava p s =
        let s' = runTokenizer s
        in case parse p "" s' of
               Right r -> r
               Left l -> error (show l)
