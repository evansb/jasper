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

type TypeParams = [TypeParam]

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

data SuperClass = Extends ClassType
                PRODUCTION

type InterfaceTypeList = [ClassType]
data SuperInterfaces = Implements InterfaceTypeList
                     PRODUCTION

data ClassDeclaration = Class [ClassModifier] Ident
                      (Maybe [TypeParam]) (Maybe SuperClass)
                      (Maybe SuperInterfaces) ClassBody
                      PRODUCTION

data Modifier = Public    | Protected    | Private   | Abstract
              | Static    | Final        | StrictFP  | Volatile
              | Transient | Synchronized | Native    | Interface
              | Default
              PRODUCTION

type ClassModifier = Modifier

data ClassBodyDeclaration = ClassMemberDeclaration ClassMemberDeclaration
                          | InstanceInitializer Block
                          | StaticInitializer Block
                          | ConstructorDeclaration [ConstructorModifier]
                                ConstructorDeclarator (Maybe Throws)
                                ConstructorBody
                          PRODUCTION

type UnannType = Type

data ClassMemberDeclaration =
            FieldDeclaration  [FieldModifier] UnannType VariableDeclaratorList
          | MethodDeclaration [MethodModifier] MethodHeader MethodBody
          | MemberClassDeclaration ClassDeclaration
          | MemberInterfaceDeclaration InterfaceDeclaration
          | EmptyClassMember
          PRODUCTION

type FieldModifier = Modifier

type VariableDeclaratorList = [VariableDeclarator]

data VariableDeclarator =
        VariableDeclarator VariableDeclID (Maybe VariableInitializer)
        PRODUCTION

type VariableDeclID = (Ident, Dims)

type MethodModifier = Modifier

data Result = RType UnannType
            | RVoid
            PRODUCTION

data MethodHeader = MethodHeader Result MethodDeclarator (Maybe Throws)
                  | MethodHeaderTP TypeParams Result MethodDeclarator
                    (Maybe Throws)
                  PRODUCTION

data MethodDeclarator = MethodDeclarator Ident
                      (Maybe FormalParameterList) Dims
                      PRODUCTION

data Throws = Throws ExceptionTypeList
            PRODUCTION

type ExceptionTypeList = [ExceptionType]

data ExceptionType = ClassTypeEx ClassType
                   | TypeVariableEx TypeVariable
                   PRODUCTION

data MethodBody = MethodBody Block
                | EmptyBody
                PRODUCTION

type InstanceInitializer = Block
type StaticInitializer = Block

type FormalParameterList = [FormalParameter]

data FormalParameter =
             FormalParameter
                [VariableModifier] UnannType VariableDeclID
            -- TODO Change VariableModifier to Annotation
             | ReceiverParameter
                [VariableModifier] UnannType (Maybe Ident)
            -- TODO Change VariableModifier to Annotation
             | EllipsisParameter
                [VariableModifier] UnannType [VariableModifier] VariableDeclID
             PRODUCTION

-- TODO Add Annotation
data VariableModifier = FinalV
                      | VoidV
                      PRODUCTION

data VariableInitializer = Expression Expression
                         | ArrayInitializer ArrayInitializer
                         PRODUCTION

-- data ConstructorDeclaration (See ClassMemberDec)

type ConstructorModifier = Modifier

data ConstructorDeclarator = ConstructorDeclarator (Maybe TypeParams)
                           SimpleTypeName (Maybe FormalParameterList)
                           PRODUCTION

type SimpleTypeName = Ident

data ConstructorBody = ConstructorBody
                     (Maybe ExplicitConstructorInvocation)
                     (Maybe BlockStatements)
                     PRODUCTION

data ExplicitConstructorInvocation =
                    ThisECI (Maybe TypeArgs) (Maybe ArgList)
                  | SuperECI (Maybe TypeArgs) (Maybe ArgList)
                  | NameSuperECI TypeName (Maybe TypeArgs) (Maybe ArgList)
                  | PrimarySuperECI Primary (Maybe TypeArgs) (Maybe ArgList)
                  PRODUCTION

data EnumDeclaration = EnumDeclaration [ClassModifier]
                       Ident (Maybe SuperInterfaces) EnumBody
                     PRODUCTION

data EnumBody = EnumBody (Maybe EnumConstantList) (Maybe EnumBodyDeclarations)
              PRODUCTION

data EnumConstant = EnumConstant [EnumConstantModifier] Ident
                        (Maybe ArgList) (Maybe ClassBody)
                  PRODUCTION

type EnumConstantList = [EnumConstant]

type EnumBodyDeclarations = [ClassBodyDeclaration]

-- | TODO Change to annotation
data EnumConstantModifier = EnumConstantModifier
                          PRODUCTION

-- | 9. Interfaces
data InterfaceDeclaration = NormalInterface [InterfaceModifier] Ident
                            (Maybe TypeParams) (Maybe ExtendsInterfaces)
                             InterfaceBody
                          PRODUCTION

type InterfaceModifier = Modifier

data ExtendsInterfaces = ExtendsInterfaces InterfaceTypeList
                       PRODUCTION

type InterfaceBody = [InterfaceMemberDeclaration]

data InterfaceMemberDeclaration =
          ConstantDeclaration [ConstantModifier] UnannType
           VariableDeclaratorList
        | InterfaceMethodDeclaration [InterfaceMethodModifier]
            MethodHeader MethodBody
        | InterfaceClassDeclaration ClassDeclaration
        | InnerInterfaceDeclaration InterfaceDeclaration
        PRODUCTION

type ConstantModifier = Modifier

data BlockStatements = BlockStatements
                     PRODUCTION

type InterfaceMethodModifier = Modifier

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

data LambdaParameters = LIdent Ident
                      | LFormalParameterList FormalParameterList
                      | LInferredFormalParameterList [Ident]
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
