{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Expression where

import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
import Text.Parsec.Combinator
import Text.Parsec.Prim

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.Parser.Type (typeArg)
import Language.Java.AST

-- | Java Expressions
expression :: JParser Expression
expression = choice (map try [
        literal
     ,  this
     ,  typeNameDotThis
     ,  typeNameDotClass
     ,  typeNameArrDotClass
     ,  voidDotClass
     ,  expressionParen
     ]) <?> "expression"

-- | Expression surrounded by parenthesis
expressionParen :: JParser Expression
expressionParen = between lParen rParen expression

-- | Literals
literal :: JParser Expression
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
typeNameDotClass :: JParser Expression
typeNameDotClass = TypeNameDotClass
        <$> typeNameDot
        <* keyword "class"
        <?> "typename.class"

-- | Java array name followed by a .class
typeNameArrDotClass :: JParser Expression
typeNameArrDotClass = TypeNameArrDotClass
        <$> typeName
        <*> (length <$> many (lSquare >> rSquare))
        <*  dot <* keyword "class"
        <?> "typename[].class"

-- | Parses void.class
voidDotClass :: JParser Expression
voidDotClass = pure VoidDotClass
        <* keyword "void"
        <* dot <* keyword "class"
        <?> "void.class"

-- | Java name followed by this
-- | e.g foo.bar.this
typeNameDotThis :: JParser Expression
typeNameDotThis = TypeNameDotThis
        <$> typeNameDot 
        <* keyword "this"

-- | The literal this expression
this :: JParser Expression
this = pure This <* keyword "this"

-- | Class instance creation expression
classInstanceCreationExpression :: JParser Expression
classInstanceCreationExpression = ClassInstanceCreationExpression <$>
     choice (map try [withIdentifier, withExpressionName, withPrimary])

withIdentifier :: JParser ClassInstanceCreation         
withIdentifier = WithIdentifier
        <$> (keyword "new" *> optionMaybe typeArg)
        <*> ident
        <*> typeArgOrDiamond
        <*> between lParen rParen (optionMaybe argumentList)
        <*> optionMaybe classBody

withExpressionName :: JParser ClassInstanceCreation
withExpressionName = WithExpressionName
        <$> typeName
        <*> (dot *> keyword "new" *> optionMaybe typeArg)
        <*> typeArgOrDiamond
        <*> between lParen rParen (optionMaybe argumentList)
        <*> optionMaybe classBody

withPrimary :: JParser ClassInstanceCreation
withPrimary = WithPrimary
        <$> expression
        <*> (dot *> keyword "new" *> optionMaybe typeArg)
        <*> ident
        <*> typeArgOrDiamond
        <*> between lParen rParen (optionMaybe argumentList)
        <*> optionMaybe classBody

typeArgOrDiamond :: JParser TypeArgOrDiam
typeArgOrDiamond =  try (TypeArg <$> typeArg)
                <|> (pure Diamond <* lessThan <* greaterThan)

argumentList :: JParser ArgList
argumentList = undefined

classBody :: JParser ClassBody
classBody = undefined

fieldAccess :: JParser FieldAccess
fieldAccess = choice $ map try
        [ ExprFieldAccess <$> (expression <* dot) <*> ident
        , SelfParentFieldAccess <$>  (keyword "super" *> dot *> ident)
        , ParentFieldAccess <$> (typeNameDot <* keyword "super") <*> ident
        ]
