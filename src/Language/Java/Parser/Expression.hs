{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Expression where

import Control.Applicative ((<$>), (<*))
import Text.Parsec.Combinator
import Text.Parsec.Prim

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.AST

expression :: JParser Expression
expression = choice (map try [
        Literal <$> literal
     ,  this
     ,  typeNameDotThis
     ,  typeNameDotClass
     ,  typeNameArrDotClass
     ,  voidDotClass
     ,  expressionParen
     ]) <?> "expression"

expressionParen :: JParser Expression
expressionParen = between lParen rParen expression

literal :: JParser Literal
literal = do
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

typeNameDotClass :: JParser Expression
typeNameDotClass = do
        typeName0 <- typeNameDot
        _ <- keyword "class"
        return $ TypeNameDotClass typeName0

typeNameArrDotClass :: JParser Expression
typeNameArrDotClass = do
        typeName0 <- typeName
        arr <- many (lSquare >> rSquare)
        _ <- dot >> keyword "class"
        return $ TypeNameArrDotClass (length arr) typeName0

voidDotClass :: JParser Expression
voidDotClass =
        keyword "void" >> dot >> keyword "class" >>
        return VoidDotClass

typeNameDotThis :: JParser Expression
typeNameDotThis =
    TypeNameDotThis <$>
        (typeNameDot <* keyword "this")

this :: JParser Expression
this = keyword "this" >> return This
