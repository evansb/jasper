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

-- | Names and Identifiers

-- | Java Identifier [ident]
data Ident = Ident String
           DERIVE

-- | Identifiers separated by period [name]
--   e.g foo.bar.baz
data TypeName = TypeName [Ident]
          DERIVE

-- | Type annotations [type_]
data Type = PrimType PrimType
          | RefType RefType
          DERIVE

-- | Primitive types [primType]
data PrimType
    = BooleanT
    | ByteT
    | ShortT
    | IntT
    | LongT
    | CharT
    | FloatT
    | DoubleT
    DERIVE

type ClassType = (Ident, Maybe [TypeArg])

-- | Reference types [refType]
data RefType = ClassOrInterfaceType [ClassType]
             | ArrayType ArrayType
             DERIVE

data TypeArg = ActualType RefType
             | Wildcard (Maybe WildcardBound)
             DERIVE

data WildcardBound = SuperWB RefType
                   | ExtendsWB RefType
                   DERIVE

data ArrayType = PrimArrayT PrimType
               | RefArrayT RefType
               DERIVE

-- | 7. Packages

data CompilationUnit = CompilationUnit (Maybe PackageDeclaration) 
                                [ImportDeclaration] [TypeDeclaration]
                     DERIVE

data PackageDeclaration = PackageDeclaration [PackageModifier] TypeName
                        DERIVE

data PackageModifier = PackageModifier
                     DERIVE

data ImportDeclaration = SingleTypeImportDeclaration TypeName
                       | TypeImportOnDemandDeclaration TypeName
                       | SingleStaticImportDeclaration TypeName Ident
                       | StaticImportOnDemandDeclaration TypeName
                       DERIVE

data TypeDeclaration = ClassDeclaration
                     | InterfaceDeclaration
                     | EmptyStatement
                     DERIVE

-- | Literals
data Literal = IntegerLiteral Integer
             | FloatingPointLiteral String
             | BooleanLiteral Bool
             | CharacterLiteral Char
             | StringLiteral String
             | NullLiteral
             DERIVE

-- | Expressions
data Expression = Literal Literal
                -- | foo.class
                | TypeNameDotClass TypeName
                -- | foo[][].class
                | TypeNameArrDotClass Int TypeName
                -- | void.class
                | VoidDotClass
                -- | this
                | This
                -- | foo.this
                | TypeNameDotThis TypeName
                DERIVE
