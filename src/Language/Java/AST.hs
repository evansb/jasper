{-# LANGUAGE CPP #-}
module Language.Java.AST where

import Text.Parsec

#define DERIVE deriving(Eq,Show)

type Token = (T, SourcePos)
data T  = Identifier String
        | Keyword    String
        | Operator   String

        | TokInt     Integer
        | TokLong    Integer
        | TokDouble  String
        | TokFloat   String
        | TokChar    Char
        | TokString  String
        | TokBool    Bool
        | TokNull
        | TokIdent   String

        | LParen     | RParen
        | LBrace     | RBrace
        | LSquare    | RSquare
        | SemiColon  | Comma
        | Period     | TPeriod
        | At         | DColon
        DERIVE

data PrimitiveType = NumericType NumericTypeKeyword
                   | BooleanType
                   DERIVE

data WildcardBound = Extend | Super
                   DERIVE

data Wildcard = Wildcard [(WildcardBound, ReferenceType)]
              DERIVE

data ReferenceType = ClassType [Either ReferenceType Wildcard]
                   DERIVE

data NumericTypeKeyword = JByte | JShort | JInt | JLong | JChar |
                          JFloat | JDouble
                        DERIVE
