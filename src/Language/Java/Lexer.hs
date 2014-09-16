
module Language.Java.Lexer where

import Control.Applicative ((<*), (*>))

import Text.Parsec
import Text.Parsec.Token hiding (identifier, stringLiteral, charLiteral, dot)
import Text.Parsec.String

import Language.Java.Lexer.Internal
import Language.Java.AST

nextToken :: Parser Token
nextToken = choice $ map (lexeme javaLexer . try)
        [ comm, period, at, lParen, rParen, lSquare, rSquare, lBrace, rBrace,
          semiColon, tokNull, tokBool, tokString, tokChar, identOrKeyword,
          tokDouble, tokInt, dColon, op ]

tokenize :: Parser [Token]
tokenize = whiteSpace javaLexer *> many nextToken <* eof

runTokenizer :: String -> [Token]
runTokenizer s = case parse tokenize "" s of
                   Left pe -> error (show pe)
                   Right toks -> toks
