{-# LANGUAGE CPP #-}
module Language.Java.AST where

import Text.Parsec

#define PRODUCTION deriving(Eq, Show)

type Token = (T, SourcePos)
data T  = Keyword    String
        | Operator   String
        -- Operators
        | OpIncr    | OpDecr    | OpPlus  | OpMinus  | OpTilde
        | OpExcl    | OpMult    | OpDiv   | OpMod    | OpRShift
        | OpURShift | OpLShift  | OpLT    | OpGT     | OpGTEq
        | OpLTEq    | OpIOf     | OpEqEq  | OpNotEq  | OpBitAnd
        | OpOr      | OpBitOr   | OpBitXor
        -- Literal tokens
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

-- | Primitive types or Reference Types. [type_]
data Type = PrimType PrimType
          | RefType RefType
          PRODUCTION

-- | Primitive types. [primType]
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

-- | Classes type are either simple class type or aggregate.
--   ClassType       Container<T>
--   AggrClassType   java.util.Container<T>
data ClassType = ClassType Ident (Maybe TypeArgs)
               | AggrClassType [ClassType] Ident (Maybe TypeArgs)
               PRODUCTION

-- | Reference types [refType]
-- There is no significant difference between class and interface type.
data RefType = ClassOrInterfaceType ClassType
             | ArrayType ArrayType
             PRODUCTION

type TypeVariable = Ident

data TypeArg = ActualType RefType
             | Wildcard (Maybe WildcardBound)
             PRODUCTION

type TypeArgs = [TypeArg]

data WildcardBound = SuperWB RefType
                   | ExtendsWB RefType
                   PRODUCTION

data ArrayType = PrimArrayT PrimType Dims
               | RefArrayT RefType Dims
               | TypeVarArrayT TypeVariable Dims
               PRODUCTION

type Dims = Int

data TypeParam = TypeParam Ident (Maybe TypeBound)
               PRODUCTION

data TypeBound = ExtendsTypeVar TypeVariable
               | ExtendsClassType ClassType [ClassType]
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

-- | 8. Classes

-- | 15. Expressions

data Expression = LambdaExpression LambdaParameters LambdaBody
                | AssignmentExpression
                PRODUCTION

-- | Literals [literal]
data Literal = IntegerLiteral Integer
             | FloatingPointLiteral String
             | BooleanLiteral Bool
             | CharacterLiteral Char
             | StringLiteral String
             | NullLiteral
             PRODUCTION

-- | Primary [primary]
data Primary = Literal Literal
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
                | FieldAccess FieldAccess
                -- | Array access
                | ArrayAccess ArrayAccess
                -- | Method invocation
                | MethodInvocation MethodInvocation
                -- | Method reference
                | MethodReference MethodReference
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
data FieldAccess = ExprFieldAccess Primary Ident
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
        PRODUCTION

data ArgList = ArgList [Expression]
               PRODUCTION

data MethodReference = NameMR TypeName (Maybe TypeArgs) Ident
                     | RefTypeMR RefType (Maybe TypeArgs) Ident
                     | ExprMR  Expression (Maybe TypeArgs) Ident
                     | SelfParentMR (Maybe TypeArgs) Ident
                     | ParentMR TypeName (Maybe TypeArgs) Ident
                     | ClassTypeMR ClassType (Maybe TypeArgs)
                     | ArrayTypeMR ArrayType
                     PRODUCTION

data DimExpr = DimExpr Expression
             PRODUCTION

type DimExprs = [DimExpr]

data ArrayCreationExpr =
         PrimTypeACE PrimType DimExprs Int
       | ClassTypeACE ClassType DimExprs Int
       | PrimTypeACEI PrimType Int ArrayInitializer
       | ClassTypeACEI ClassType Int ArrayInitializer
       PRODUCTION

type ArrayInitializer = [VariableInitializer]

type ConstantExpression = Expression

data VariableInitializer = Expression Expression
                         | ArrayInitializer ArrayInitializer
                         PRODUCTION

data LambdaParameters = LPIdent Ident
                      | FormalParameterList [Ident]
                      | InferredFormalParameterList [Ident]
                      PRODUCTION

data LambdaBody = LambdaBodyExpression Expression
                | LambdaBodyBlock      Block
                PRODUCTION

data LHS = LHSExpr  Primary
         | LHSIdent Ident
         PRODUCTION

data Assignment = Assignment LHS T Expression
                PRODUCTION

data Block = Block
           PRODUCTION

data PostfixExpr = PrimPostfixExpr Primary
                 | NamePostfixExpr TypeName
                 | PostIncrementExpr PostfixExpr
                 | PostDecrementExpr PostfixExpr
                 PRODUCTION

data UnaryExpression = PreIncrementExpr UnaryExpression
                     | PreDecrementExpr UnaryExpression
                     | UnaryPlus UnaryExpression
                     | UnaryMinus UnaryExpression
                     | UnaryNPM UnaryExpressionNotPlusMinus
                     PRODUCTION

data UnaryExpressionNotPlusMinus = UnaryPostfix PostfixExpr
                                 | UnaryTilde UnaryExpression
                                 | UnaryNegate UnaryExpression
                                 | UnaryCast CastExpression
                                 PRODUCTION

data CastExpression = UnaryToPrim PrimType UnaryExpression
                    | UnaryToRef RefType [ClassType] UnaryExpressionNotPlusMinus
                    | LambdaToRef RefType [ClassType] Expression
                    PRODUCTION

data CondExpr = CondExpr Expression Expression Expression
              PRODUCTION

data OpExpr = PrimExpr Primary
            | NameExpr TypeName
            | PrefixExpr T OpExpr
            | PostfixExpr T OpExpr
            | BinaryExpr T OpExpr OpExpr
            PRODUCTION
