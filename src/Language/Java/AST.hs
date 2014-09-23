{-# LANGUAGE CPP #-}
module Language.Java.AST where

import Text.Parsec

#define DERIVE deriving(Eq,Show)

type Token = (T, SourcePos)
data T  = Keyword    String
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

--- | Names and Identifiers

-- | A single identifier [ident]
---  e.g foo
data Ident = Ident String
           DERIVE

-- | Identifiers separated by period [name]
--   e.g foo.bar.baz
data Name = Name [Ident]
          DERIVE
