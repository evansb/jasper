{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Expression where

import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Expr

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.Parser.Type
import Language.Java.AST


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

expression :: JParser Expression
expression = undefined

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
opExpr = choice
       [ PrimExpr <$> primary
       , NameExpr <$> typeName
       ]

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
