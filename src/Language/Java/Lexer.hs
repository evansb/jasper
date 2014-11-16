{-|
Module      : Language.Java.Lexer.Internal
Description : Lexer for Java program.
Copyright   : (c) Evan Sebastian 2014
License     : MIT
Maintainer  : evan.sebastian@u.nus.edu
Stability   : experimental
Portability : GHC 7.8.2

This module defines lexer for Java program.
Use lexJava to break a string into a list of 'Token's.

>>> lexJava "public class Main { }"

-}

module Language.Java.Lexer where

import Control.Applicative ((<*), (*>))

import Text.Parsec
import Text.Parsec.Token hiding (identifier, stringLiteral, charLiteral, dot)
import Text.Parsec.String

import Language.Java.Lexer.Internal
import Language.Java.AST

lexJava :: String -> [Token]
lexJava = runTokenizer

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
