{-|
Module      : Language.Java.Lexer.Internal
Description : Internal implementation of the lexical analysis.
Copyright   : (c) Evan Sebastian 2014
License     : MIT
Maintainer  : evan.sebastian@u.nus.edu
Stability   : experimental
Portability : GHC 7.8.2

This module implements lexer from String to a list of Token.
Consider using 'Language.Java.Lexer' instead of this module.

-}

{-# LANGUAGE DoAndIfThenElse #-}
module Language.Java.Lexer.Internal where

import Control.Monad.Identity (Identity)
import Control.Applicative ((<$>), (<*), (*>), (<*>))

import qualified Data.HashSet as S
import Data.Char (chr, ord, isHexDigit, readLitChar, isAlpha)

import Text.Parsec
import Text.Parsec.Token hiding (identifier, stringLiteral, charLiteral, dot)
import Text.Parsec.Language
import Text.Parsec.String

import Language.Java.AST

javaReservedNames = [
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char",
    "class", "const", "continue", "default", "do", "double", "else", "enum",
    "extends", "final", "finally", "float", "for", "goto", "if", "implements",
    "import", "instanceof", "int", "interface", "long", "native", "new",
    "package", "private", "protected", "public", "return", "short", "static",
    "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
    "transient", "try", "void", "volatile", "while"]

javaReservedOpNames = [
    "!=", "!", ">>>=", ">>>", ">>=", ">=", ">>", ">",
    "==", "=", "|", "|=", "||", "&&", "&=", "&", "^=", "^",
    "%", "%=", "*=", "*", "++", "+=", "+", "--", "-=", "-",
    "/=", "/", ":", "<<=", "<<", "<=", "<",
    "?"]

keywordTable = S.fromList javaReservedNames
operatorTable = S.fromList javaReservedOpNames
isKeyword = flip S.member keywordTable
isOperator = flip S.member operatorTable

javaLanguage :: LanguageDef st
javaLanguage = javaStyle {
    reservedNames     = javaReservedNames,
    reservedOpNames   = javaReservedOpNames,
    caseSensitive     = True,
    opStart           = oneOf "!%&*+/<=>?^|-:.",
    opLetter          = oneOf "&*+/=^|-"
}

javaLexer :: GenTokenParser String u Identity
javaLexer = makeTokenParser javaLanguage

identOrKeyword :: Parser Token
identOrKeyword = do
    p <- getPosition
    s <- identifier
    return $ if isKeyword s
        then (Keyword s, p)
        else (TokIdent s, p)

tokInt      =  TokInt       <=+ integerLiteral
tokDouble   =  TokFloat     <=+ floatLiteral
tokString   =  TokString    <=+ stringLiteral
tokChar     =  TokChar      <=+ charLiteral
tokBool     =  TokBool      <=+ boolLiteral
tokNull     =  TokNull      <=* string "null"
op          =  Operator     <=+ opLiteral
lParen      =  LParen       <=* char '('
rParen      =  RParen       <=* char ')'
lSquare     =  LSquare      <=* char '['
rSquare     =  RSquare      <=* char ']'
lBrace      =  LBrace       <=* char '{'
rBrace      =  RBrace       <=* char '}'
semiColon   =  SemiColon    <=* char ';'
at          =  At           <=* char '@'
comm        =  Comma        <=* char ','
period      =  Period       <=* (char '.' <* notFollowedBy digit)
dColon      =  DColon       <=* string "::"

javaSingleChar :: Parser Char
javaSingleChar = noneOf "\\'" <|> unicodeEscape

unicodeEscape :: Parser Char
unicodeEscape = do
    _ <- string "\\u"
    cs <- count 4 anyChar
    if all isHexDigit cs then
        return ((fst . head) $ readLitChar ("\\x" ++ cs))
    else
        parserFail "Bad unicode escape"

escapeSequence :: Parser Char
escapeSequence =
    convChar <$> try (char '\\' *> oneOf "btnfr\"'\\") <|> octalEscape
        where convChar c = case c of {
            'b' -> '\b'; 't' -> '\t';
            'n' -> '\n'; 'f' -> '\f';
            'r' -> '\r';  x -> x;
        }

octalEscape :: Parser Char
octalEscape =
    char '\\' *> (choice . map try)
       [readOctal  <$> octalDigit <* eof,
        readOctal2 <$> octalDigit  <*> octalDigit <* eof,
        readOctal3 <$> zeroToThree <*> octalDigit <*> octalDigit]
        where
            zeroToThree       = drange '0' '3'
            octalDigit        = drange '0' '7'
            readOctal  s      = chr (read ("0o" ++ [s]) :: Int)
            readOctal2 s t    = chr (read ("0o" ++ [s, t]) :: Int)
            readOctal3 s t u  = chr (read ("0o" ++ [s, t, u]) ::Int)

drange :: Char -> Char -> Parser Char
drange b e = satisfy (\c -> ord b <= ord c && ord c <= ord e)

stringLiteral :: Parser String
stringLiteral =
    let stringCharacter = satisfy (\c -> c /= '"' && c /= '\\') in
    char '"' *> many (stringCharacter <|> escapeSequence) <* char '"'

charLiteral :: Parser Char
charLiteral = char '\'' *> (try escapeSequence <|> javaSingleChar) <* char '\''

javaLetter :: Parser Char
javaLetter = satisfy (\c -> isAlpha c || c == '$' || c == '_')

identifier :: Parser String
identifier = (:) <$> javaLetter <*> many (alphaNum <|> oneOf "_$")

integerLiteral :: Parser Integer
integerLiteral = (read <$> (choice . map try)
    [ hexNumeral, binaryNumeral, octalNumeral, decimalNumeral ])
    <* notFollowedBy (char '.')

decimalNumeral =  string "0" <* notFollowedBy digit
              <|> ((:) <$> drange '1' '9' <* underscore <*>
                    many (digit <* underscore))

octalNumeral = ("0o" ++) <$> (string "0" *> many1 octDigit)

binaryNumeral = bin2dec <$> (string "0b" *> many1 (oneOf "01" <* underscore))

bin2dec = show . foldr (\c s -> s * 2 + c::Integer) 0 . reverse . map c2i
            where c2i c = if c == '0' then 0 else 1

hexNumeral = (++) <$> string "0x" <*> many1 (hexDigit <* underscore)
underscore = optional (many (char '_'))

floatLiteral :: Parser String
floatLiteral = decimalFloatLiteral <|> hexFloatLiteral where
    decimalFloatLiteral = choice $ map try
     [
        a5 <$> digits <*> dot <*> can digits <*> can ex <*> can suffix
      , a4 <$> dot <*> digits <*> can ex <*> can suffix
      , a3 <$> digits <*> ex <*> can suffix
      , a3 <$> digits <*> can ex <*> suffix
     ]

    hexFloatLiteral = (++) <$> hexSignificand <*> binex
    hexSignificand = choice $ map try
     [
        (++) <$> hexNumeral <*> can dot
      , a6 <$> char '0' <*> oneOf "xX" <*> can hexDigits <*> dot <*> hexDigits
     ]
    binex = a3 <$> pp <*> can sign <*> many1 digit
    hexDigits = many1 hexDigit
    digits = many1 digit
    dot = string "."
    ex = (\a b c -> a : (b ++ c)) <$> oneOf "eE" <*> can sign <*> many1 digit
    suffix = (:[]) <$> oneOf "fFdD"
    sign = (:[]) <$> oneOf "+-"
    pp = (:[]) <$> oneOf "pP"
    can p = try p <|> return ""
    a4 a b c d = a ++ b ++ c ++ d
    a3 a b c = a ++ b ++ c
    a5 a b c d e = a ++ b ++ c ++ d ++ e
    a6 a b c d e = a:b:(c ++ d ++ e)

boolLiteral :: Parser Bool
boolLiteral =
    readBool <$> (choice . map string) ["true", "false"]
        where readBool ('t':_) = True
              readBool _      = False

opLiteral :: Parser String
opLiteral = do
    ohead <- opStart javaLanguage
    obody <- many $ opLetter javaLanguage
    let o = ohead : obody
    if isOperator o then
        return o
    else
        unexpected ("Unknown operator" ++ o)

(<=+) :: (a -> T) -> Parser a -> Parser Token
t <=+ p = do
    pos <- getPosition
    m <- p
    return (t m, pos)

(<=*) :: T -> Parser a -> Parser Token
t <=* p = do
    pos <- getPosition
    _ <- p
    return (t, pos)
