{-# LANGUAGE DoAndIfThenElse #-}
module Language.Java.Parser.Internal where

import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)
import qualified Data.Set as S hiding (map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Misc

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Expr

import Language.Java.AST
import Language.Java.Parser.Core

-- | Java Identifier
ident :: JParser Ident
ident = Ident <$> (getSS <$> satisfy isIdentifier)

-- | Java Name
typeName :: JParser TypeName
typeName = TypeName <$> ident `sepBy1` dot

-- | Java Name Dot, for parsing Name dot class
typeNameDot :: JParser TypeName
typeNameDot = TypeName <$> ident `sepEndBy1` dot

--------------------------------------
-- | Productions from §4 (Types, Values, and Variables)
--------------------------------------

-- | Java types are either primitive or reference types.
type_ :: JParser Type
type_ = try (PrimType <$> primType)
     <|> (RefType <$> refType)

-- | Primitive types.
primType :: JParser PrimType
primType = (do
    let isPrimType x = isKeyword x && S.member (getSS x) primitiveTypes
    tok <- satisfy isPrimType
    return $ case getSS tok of
      "byte"    -> ByteT
      "short"   -> ShortT
      "int"     -> IntT
      "long"    -> LongT
      "char"    -> CharT
      "float"   -> FloatT
      "double"  -> DoubleT
      _         -> BooleanT)
    <?> "primitive type"

-- | Reference types.
refType :: JParser RefType
refType = (try classOrInterfaceT <|> (ArrayType <$> arrayType))
       <?> "reference type"

classOrInterfaceT :: JParser RefType
classOrInterfaceT = ClassOrInterfaceType <$> classType

classType :: JParser ClassType
classType = ClassType <$> ident <*> optionMaybe typeArgs
         <?> "class type"

simpleClassType :: JParser ClassType
simpleClassType = ClassType <$> ident <*> optionMaybe typeArgs

aggrClassType :: JParser ClassType
aggrClassType = AggrClassType
             <$> (many classType <* dot)
             <*> ident
             <*> optionMaybe typeArgs

arrayDims :: JParser Int
arrayDims = length <$> many (lSquare <* rSquare)

arrayType :: JParser ArrayType
arrayType = choice (map try
          [ primArrayType
          , refArrayType
          , typeVarArrayType
          ]) <?> "array types"

typeVariable :: JParser TypeVariable
typeVariable = ident

primArrayType :: JParser ArrayType
primArrayType = PrimArrayT <$> primType <*> arrayDims

refArrayType :: JParser ArrayType
refArrayType = RefArrayT <$> classOrInterfaceT <*> arrayDims

typeVarArrayType :: JParser ArrayType
typeVarArrayType = TypeVarArrayT <$> typeVariable <*> arrayDims

typeParams :: JParser [TypeParam]
typeParams = between lessThan greaterThan (typeParam `sepBy1` comma)

typeParam :: JParser TypeParam
typeParam = TypeParam <$> ident <*> optionMaybe typeBound

typeBound :: JParser TypeBound
typeBound = keyword "extends" *> choice (map try
          [ ExtendsTypeVar   <$> typeVariable
          , ExtendsClassType <$> classType <*> additionalBound
          ])

additionalBound :: JParser [ClassType]
additionalBound = many (operator "&" *> classType)

typeArgs :: JParser [TypeArg]
typeArgs = between lessThan greaterThan (typeArg `sepBy1` comma)
        <?> "type arguments"

typeArg :: JParser TypeArg
typeArg = do
    let wc x = isOperator x && (x === "?")
    tok <- try (getSS <$> satisfy wc) <|> pure ""
    if null tok then
        ActualType <$> refType
    else
        Wildcard <$> optionMaybe wildcardBound
    ; <?> "type argument"

-- | Wildcard bounds
wildcardBound :: JParser WildcardBound
wildcardBound = SuperWB <$> (keyword "super" *> refType)
             <|> ExtendsWB <$> (keyword "extends" *> refType)
             <?> "wild card bound"

-- | Miscellaneous functions
primitiveTypes = S.fromList
      [ "byte" , "short" , "int" , "long" , "char",
        "float" , "double" , "boolean"]

--------------------------------------
-- | Productions from §7 (Packages)
--------------------------------------

-- | Compilation unit
compilationUnit :: JParser CompilationUnit
compilationUnit = CompilationUnit
        <$> optionMaybe packageDeclaration
        <*> many importDeclaration
        <*> many typeDeclaration
        <?> "compilation unit"

-- | Package declaration
packageDeclaration :: JParser PackageDeclaration
packageDeclaration = PackageDeclaration
        <$> many packageModifier
        <*> typeName
        <*  semiColon
        <?> "package declaration"

-- | Package Modifier = Annotation
-- | TODO Change this once Annotation is finished
packageModifier :: JParser PackageModifier
packageModifier = return PackageModifier
               <?> "package modifier"

-- | Import declaration
importDeclaration :: JParser ImportDeclaration
importDeclaration = choice (map try [
          singleTypeImportDeclaration
        , typeImportOnDemandDeclaration
        , singleStaticImportDeclaration
        , staticImportOnDemandDeclaration
        ])
        <?> "import declaration"

singleTypeImportDeclaration :: JParser ImportDeclaration
singleTypeImportDeclaration = SingleTypeImportDeclaration
        <$> (keyword "import" *> typeName)
        <* semiColon

typeImportOnDemandDeclaration :: JParser ImportDeclaration
typeImportOnDemandDeclaration = TypeImportOnDemandDeclaration
        <$> (keyword "import" *> typeNameDot <* star <* semiColon)

singleStaticImportDeclaration :: JParser ImportDeclaration
singleStaticImportDeclaration = do
        tn <- keyword "import" *> keyword "static" *> typeNameDot <* semiColon
        case tn of
            TypeName s@(_:_:_) -> do
                    let (x,y) = splitAt (length s - 1) s
                    return $ SingleStaticImportDeclaration (TypeName x) (head y)
            _ -> unexpected "Cannot import whole class"

staticImportOnDemandDeclaration :: JParser ImportDeclaration
staticImportOnDemandDeclaration = StaticImportOnDemandDeclaration
        <$> (keyword "import" *> keyword "static" *> typeNameDot <*
            (star <* semiColon))

-- | Productions from §8 (Classes)
superClass :: JParser SuperClass
superClass = keyword "extends" *> classType

superInterfaces :: JParser SuperInterfaces
superInterfaces = keyword "implements" *> (classType `sepBy1` comma)

classDeclaration :: JParser ClassDeclaration
classDeclaration = normalClassDeclaration

classModifiers :: JParser [ClassModifier]
classModifiers = many classModifier

-- TODO Add Annotation
classModifier :: JParser ClassModifier
classModifier = do
        tok <- getSS <$> getT
        case M.lookup tok classModifierTable of
            Just modifier -> return modifier
            Nothing -> unexpected "class modifier"

classBody :: JParser ClassBody
classBody = error "not implemented"

normalClassDeclaration :: JParser ClassDeclaration
normalClassDeclaration = Class
                      <$> classModifiers
                      <*> (keyword "class" *> ident)
                      <*> optionMaybe typeParams
                      <*> optionMaybe superClass
                      <*> optionMaybe superInterfaces
                      <*> classBody
                      <?> "normal class declaration"

variableDeclarator :: JParser VariableDeclarator
variableDeclarator = VariableDeclarator
                  <$> variableDeclaratorID
                  <*> optionMaybe (operator "=" *> variableInitializer)

variableDeclaratorID :: JParser VariableDeclID
variableDeclaratorID = (,) <$> ident <*> (fromMaybe 0 <$> optionMaybe arrayDims)


-- TODO Resolve this
typeDeclaration :: JParser TypeDeclaration
typeDeclaration = undefined

-- | Java Expressions
primary :: JParser Primary
primary = choice (map try [
        literal
     ,  this
     ,  typeNameDotThis
     ,  typeNameDotClass
     ,  typeNameArrDotClass
     ,  voidDotClass
{- TODO Uncomment expression prods once ready
     ,  classInstanceCreationExpression
     ,  expressionParen
     ,  fieldAccess
     ,  arrayAccess
     ,  methodInvocation
     ,  methodReference
--}
     ]) <?> "expression"

-- | TODO resolve this
expression :: JParser Expression
expression = undefined

-- | TODO resolve this
lambdaExpression :: JParser Expression
lambdaExpression = undefined

-- | Expression surrounded by parenthesis
expressionParen :: JParser Expression
expressionParen = between lParen rParen expression

-- | Literals
literal :: JParser Primary
literal = Literal <$> do
       tok <- getT
       case tok of
        TokInt t -> return $ IntegerLiteral t
        TokFloat s -> return $ FloatingPointLiteral s
        TokDouble s -> return $ FloatingPointLiteral s
        TokLong t -> return $ IntegerLiteral t
        TokChar s -> return $ CharacterLiteral s
        TokString s -> return $ StringLiteral s
        TokNull -> return NullLiteral
        s -> unexpected (show s)
       <?> "literal"

-- | Java name followed by a .class
-- | e.g foo.bar.qux.class
typeNameDotClass :: JParser Primary
typeNameDotClass = TypeNameDotClass
        <$> typeNameDot
        <* keyword "class"
        <?> "typename.class"

-- | Java array name followed by a .class
typeNameArrDotClass :: JParser Primary
typeNameArrDotClass = TypeNameArrDotClass
        <$> typeName
        <*> (length <$> many (lSquare >> rSquare))
        <*  dot <* keyword "class"
        <?> "typename[].class"

-- | Parses void.class
voidDotClass :: JParser Primary
voidDotClass = pure VoidDotClass
        <* keyword "void"
        <* dot <* keyword "class"
        <?> "void.class"

-- | Java name followed by this
-- | e.g foo.bar.this
typeNameDotThis :: JParser Primary
typeNameDotThis = TypeNameDotThis
        <$> typeNameDot
        <* keyword "this"

-- | The literal this expression
this :: JParser Primary
this = pure This <* keyword "this"

-- | Class instance creation expression
classInstanceCreationExpression :: JParser Primary
classInstanceCreationExpression = ClassInstanceCreationExpression <$>
     choice (map try [withIdentifier, withExpressionName, withPrimary])

withIdentifier :: JParser ClassInstanceCreation
withIdentifier = WithIdentifier
        <$> (keyword "new" *> optionMaybe typeArgs)
        <*> ident
        <*> typeArgsOrDiamond
        <*> between lParen rParen (optionMaybe argList)
        <*> optionMaybe classBody

withExpressionName :: JParser ClassInstanceCreation
withExpressionName = WithExpressionName
        <$> typeName
        <*> (dot *> keyword "new" *> optionMaybe typeArgs)
        <*> typeArgsOrDiamond
        <*> between lParen rParen (optionMaybe argList)
        <*> optionMaybe classBody

withPrimary :: JParser ClassInstanceCreation
withPrimary = WithPrimary
        <$> expression
        <*> (dot *> keyword "new" *> optionMaybe typeArgs)
        <*> ident
        <*> typeArgsOrDiamond
        <*> between lParen rParen (optionMaybe argList)
        <*> optionMaybe classBody

typeArgsOrDiamond :: JParser TypeArgsOrDiam
typeArgsOrDiamond =  try (TypeArgs <$> typeArgs)
                <|> (pure Diamond <* lessThan <* greaterThan)

argList :: JParser ArgList
argList = ArgList <$> expression `sepBy` comma

classBody :: JParser ClassBody
classBody = undefined

fieldAccess :: JParser Primary
fieldAccess = FieldAccess <$> choice (map try
        [ ExprFieldAccess <$> (primary <* dot) <*> ident
        , SelfParentFieldAccess <$>  (keyword "super" *> dot *> ident)
        , ParentFieldAccess <$> (typeNameDot <* keyword "super") <*> ident
        ])

arrayAccess :: JParser Primary
arrayAccess = ArrayAccess <$>
           (try (NormalArrayAccess <$> typeName
                                   <*> between lSquare rSquare expression)
           <|> (ExprArrayAccess    <$> expression
                                   <*> between lSquare rSquare expression))

methodInvocation :: JParser Primary
methodInvocation = MethodInvocation <$> choice (map try
        [ normalMethodInvocation
        , nameMethodInvocation
        , exprMethodInvocation
        , selfParentMethodInvocation
        , parentMethodInvocation
        ])

normalMethodInvocation :: JParser MethodInvocation
normalMethodInvocation = NormalMethodInvocation
        <$> typeName
        <*> between lParen rParen (optionMaybe argList)

nameMethodInvocation :: JParser MethodInvocation
nameMethodInvocation = NameMethodInvocation
        <$> typeNameDot
        <*> optionMaybe typeArgs
        <*> ident
        <*> between lParen rParen (optionMaybe argList)

exprMethodInvocation :: JParser MethodInvocation
exprMethodInvocation = ExprMethodInvocation
        <$> expression
        <*> (dot *> optionMaybe typeArgs)
        <*> ident
        <*> between lParen rParen (optionMaybe argList)

selfParentMethodInvocation :: JParser MethodInvocation
selfParentMethodInvocation = SelfParentMethodInvocation
        <$> (keyword "super" *> dot *> optionMaybe typeArgs)
        <*> ident
        <*> between lParen rParen (optionMaybe argList)

parentMethodInvocation :: JParser MethodInvocation
parentMethodInvocation = ParentMethodInvocation
        <$> typeNameDot
        <*> (keyword "super" *> dot *> optionMaybe typeArgs)
        <*> ident
        <*> between lParen rParen (optionMaybe argList)

-- TODO Resolve this
methodReference :: JParser Primary
methodReference = MethodReference <$> choice (map try
        [
        ])

nameMethodReference :: JParser MethodReference
nameMethodReference = NameMR
        <$> typeName
        <*> (dColon *> optionMaybe typeArgs)
        <*> ident

refTypeMethodReference :: JParser MethodReference
refTypeMethodReference = RefTypeMR
        <$> refType
        <*> (dColon *> optionMaybe typeArgs)
        <*> ident

exprMethodReference :: JParser MethodReference
exprMethodReference = ExprMR
        <$> expression
        <*> (dColon *> optionMaybe typeArgs)
        <*> ident

selfParentMethodReference :: JParser MethodReference
selfParentMethodReference = SelfParentMR
        <$> (keyword "super" *> dColon *> optionMaybe typeArgs)
        <*> ident

parentMethodReference :: JParser MethodReference
parentMethodReference = ParentMR
        <$> typeNameDot
        <*> (keyword "super" *> dColon *> optionMaybe typeArgs)
        <*> ident

classTypeMethodReference :: JParser MethodReference
classTypeMethodReference = ClassTypeMR
        <$> classType
        <*> (dColon *> optionMaybe typeArgs <* keyword "new")

arrayTypeMethodReference :: JParser MethodReference
arrayTypeMethodReference = ArrayTypeMR
        <$> (arrayType <* dColon <* keyword "new")

dimExprs :: JParser [DimExpr]
dimExprs = many1 (DimExpr <$> (lSquare *> expression <* rSquare))

arrayCreationExpr :: JParser ArrayCreationExpr
arrayCreationExpr = choice
         [  primTypeACE
         ,  classTypeACE
         ,  primTypeACEI
         ,  classTypeACEI
         ]
        <?> "array creation expression"

primTypeACE :: JParser ArrayCreationExpr
primTypeACE = PrimTypeACE
        <$> (keyword "new" *> primType)
        <*> dimExprs
        <*> (length <$> many dims)

classTypeACE :: JParser ArrayCreationExpr
classTypeACE = ClassTypeACE
        <$> (keyword "new" *> classType)
        <*> dimExprs
        <*> (length <$> many dims)

primTypeACEI :: JParser ArrayCreationExpr
primTypeACEI = PrimTypeACEI
        <$> (keyword "new" *> primType)
        <*> (length <$> many dims)
        <*> arrayInitializer

classTypeACEI :: JParser ArrayCreationExpr
classTypeACEI = ClassTypeACEI
        <$> (keyword "new" *> classType)
        <*> (length <$> many dims)
        <*> arrayInitializer

assignmentOperator :: JParser T
assignmentOperator = choice (map operator [
         "=", "*=", "/=", "%=", "+=", "-=", "<<=",
         ">>=", ">>>=", "&=","^=", "|="
        ])
        <?> "assignment operator"

leftHandSide :: JParser LHS
leftHandSide = try (LHSExpr <$> choice [ fieldAccess, arrayAccess ])
        <|> (LHSIdent <$> ident)
        <?> "lhs"

assignment :: JParser Assignment
assignment = Assignment
       <$> leftHandSide
       <*> assignmentOperator
       <*> expression
       <?> "assignment"

dims :: JParser ()
dims = lSquare >> rSquare >> return ()

postfixExpr :: JParser PostfixExpr
postfixExpr = choice
       [ PrimPostfixExpr <$> primary
       , NamePostfixExpr <$> typeName
       , PostIncrementExpr <$> postfixExpr <* operator "++"
       , PostDecrementExpr <$> postfixExpr <* operator "++"
       ] <?> "postfix expression"

unaryExpr :: JParser UnaryExpression
unaryExpr = choice
       [ PreIncrementExpr <$> (operator "++" *> unaryExpr)
       , PreDecrementExpr <$> (operator "--" *> unaryExpr)
       , UnaryPlus <$> (operator "+" *> unaryExpr)
       , UnaryMinus <$> (operator "-" *> unaryExpr)
       , UnaryNPM <$> unaryExprNPM
       ] <?> "unary expression"

unaryExprNPM :: JParser UnaryExpressionNotPlusMinus
unaryExprNPM = choice
       [ UnaryPostfix <$> postfixExpr
       , UnaryTilde <$> (operator "~" *> unaryExpr)
       , UnaryNegate <$> (operator "!" *> unaryExpr)
       , UnaryCast <$> castExpr
       ] <?> "unary not plus minus"

castExpr :: JParser CastExpression
castExpr = choice
       [ UnaryToPrim <$> (lParen *> primType <* rParen) <*> unaryExpr
       , UnaryToRef <$> (lParen *> refType) <*> (many classType <* rParen)
                    <*> unaryExprNPM
       , LambdaToRef <$> (lParen *> refType) <*> (many classType <* rParen)
                    <*> lambdaExpression
       ] <?> "cast expression"

opExpression :: JParser OpExpr
opExpression = buildExpressionParser table opExpr

opExpr :: JParser OpExpr
opExpr = choice [ PrimExpr <$> primary, NameExpr <$> typeName ]

table  = [ postfix <$> [ "++", "--" ]
         , prefix  <$> [ "++", "--", "+", "-", "~", "!" ]
         , binary  <$> [ "*", "/", "%" ]
         , binary  <$> [ "+", "-" ]
         , binary  <$> [ "<<", ">>", ">>>" ]
         , binary  <$> [ "<", ">", "<=", ">=", "instanceof" ]
         , binary  <$> [ "==", "!=" ]
         , binary  <$> [ "&" ]
         , binary  <$> [ "^"]
         , binary  <$> [ "|" ]
         , binary  <$> [ "&&" ]
         , binary  <$> [ "||" ]
         ]

binary op = Infix (BinaryExpr <$> operator op) AssocLeft
postfix op = Postfix (PostfixExpr <$> operator op)
prefix op = Prefix (PrefixExpr <$> operator op)

variableInitializer :: JParser VariableInitializer
variableInitializer = choice
                    [ Expression <$> expression
                    , ArrayInitializer <$> arrayInitializer
                    ] <?> "variable initializer"

arrayInitializer :: JParser ArrayInitializer
arrayInitializer = lBrace *> (variableInitializer `sepBy` comma) <* rBrace
