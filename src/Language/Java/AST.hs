{-# LANGUAGE CPP #-}
module Language.Java.AST where

import Text.Parsec

#define PRODUCTION deriving(Eq, Show)

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
        PRODUCTION

-- | Names and Identifiers

-- | Java Identifier [ident]
data Ident = Ident String
           PRODUCTION

-- | Identifiers separated by period [name]
--   e.g foo.bar.baz
data TypeName = TypeName [Ident]
          PRODUCTION

-- | Type annotations [type_]
data Type = PrimType PrimType
          | RefType RefType
          PRODUCTION

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
    PRODUCTION

type ClassType = (Ident, Maybe TypeArgs)

-- | Reference types [refType]
data RefType = ClassOrInterfaceType [ClassType]
             | ArrayType ArrayType
             PRODUCTION

data TypeArg = ActualType RefType
             | Wildcard (Maybe WildcardBound)
             PRODUCTION

type TypeArgs = [TypeArg]

data WildcardBound = SuperWB RefType
                   | ExtendsWB RefType
                   PRODUCTION

data ArrayType = PrimArrayT PrimType
               | RefArrayT RefType
               PRODUCTION

-- | 7. Packages

data CompilationUnit = CompilationUnit (Maybe PackageDeclaration) 
                                [ImportDeclaration] [TypeDeclaration]
                     PRODUCTION

data PackageDeclaration = PackageDeclaration [PackageModifier] TypeName
                        PRODUCTION

data PackageModifier = PackageModifier
                     PRODUCTION

data ImportDeclaration = SingleTypeImportDeclaration TypeName
                       | TypeImportOnDemandDeclaration TypeName
                       | SingleStaticImportDeclaration TypeName Ident
                       | StaticImportOnDemandDeclaration TypeName
                       PRODUCTION

data TypeDeclaration = ClassDeclaration
                     | InterfaceDeclaration
                     | EmptyStatement
                     PRODUCTION

-- | 15. Expressions

-- | Literals [literal]
data Literal = IntegerLiteral Integer
             | FloatingPointLiteral String
             | BooleanLiteral Bool
             | CharacterLiteral Char
             | StringLiteral String
             | NullLiteral
             PRODUCTION

-- | Expressions [expression]
data Expression = Literal Literal
                -- | foo.class
                | TypeNameDotClass TypeName
                -- | foo[][].class
                | TypeNameArrDotClass TypeName Int
                -- | void.class
                | VoidDotClass
                -- | this
                | This
                -- | foo.this
                | TypeNameDotThis TypeName
                -- | Instant class creation
                | ClassInstanceCreationExpression ClassInstanceCreation
                -- | Field access
                | FieldAccess
                -- | Array access
                | ArrayAccess
                PRODUCTION

-- | Class instance creation [classInstanceCreation]
--  e.g  new Comparable<String> {
--             <Class Body>
--  }
data ClassInstanceCreation =
       WithIdentifier (Maybe TypeArgs) Ident
                TypeArgsOrDiam (Maybe ArgList) (Maybe ClassBody)
     | WithExpressionName TypeName (Maybe TypeArgs)
                TypeArgsOrDiam (Maybe ArgList) (Maybe ClassBody)
     | WithPrimary Expression (Maybe TypeArgs) Ident
                TypeArgsOrDiam (Maybe ArgList) (Maybe ClassBody)
     PRODUCTION
 
data TypeArgsOrDiam = TypeArgs TypeArgs
                   | Diamond
                   PRODUCTION

data ClassBody = ClassBody
               PRODUCTION

-- | Field Access [fieldAccess]
--  Style 1 <expression>.field
--  Style 2 super.field
--  Style 3 <expression>.super.field
data FieldAccess = ExprFieldAccess Expression Ident
                 | SelfParentFieldAccess Ident
                 | ParentFieldAccess TypeName Ident
                 PRODUCTION
                 
-- | Array Access
-- Style 1 <name>[<expression>]
-- Style 2 <expression>[<expression>]
data ArrayAccess = NormalArrayAccess TypeName Expression
                 | ExprArrayAccess Expression Expression
                 PRODUCTION

data MethodInvocation = 
          NormalMethodInvocation TypeName (Maybe ArgList)
        | NameMethodInvocation TypeName (Maybe TypeArgs) Ident (Maybe ArgList)
        | ExprMethodInvocation Expression (Maybe TypeArgs) Ident (Maybe ArgList)
        | SelfParentMethodInvocation (Maybe TypeArgs) Ident (Maybe ArgList)
        | ParentMethodInvocation TypeName (Maybe TypeArgs) Ident (Maybe ArgList)


data ArgList = ArgList [Expression]
               PRODUCTION