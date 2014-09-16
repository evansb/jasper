
module Language.Java.AST where

import Text.Parsec

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
        deriving (Eq, Show)

data PrimitiveType = NumericType NumericTypeKeyword
                   | BooleanType
                   deriving (Eq, Show)

data WildcardBound = Extend | Super
                  deriving (Eq, Show)

data Wildcard = Wildcard [(WildcardBound, ReferenceType)]
              deriving (Eq, Show)

data ReferenceType = ClassType [Either ReferenceType Wildcard]
                   deriving (Eq, Show)

data NumericTypeKeyword = JByte | JShort | JInt | JLong | JChar |
                          JFloat | JDouble
                        deriving (Eq, Show)
