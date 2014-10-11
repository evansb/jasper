{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Expression where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Data.Maybe (isNothing)
import Text.Parsec.Combinator
import Text.Parsec.Prim

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.AST

expression :: JParser Expression
expression = between lParen rParen (choice [
        Literal <$> literal
     ,  typeNameDotClass
     ,  voidDotClass
     ,  this
     ]) <?> "expression"

literal :: JParser Literal
literal = do
       tok <- getT
       case tok of
        TokInt t -> return $ IntegerLiteral t
        TokLong t -> return $ IntegerLiteral t
        TokDouble s -> return $ FloatingPointLiteral s
        TokChar s -> return $ CharacterLiteral s
        TokString s -> return $ StringLiteral s
        TokNull -> return NullLiteral
        s -> unexpected (show s)
       <?> "literal"

typeNameDotClass :: JParser Expression
typeNameDotClass = do
        typeName0 <- typeName
        arr <- optionMaybe (lSquare >> rSquare)
        _ <- dot *> keyword "class"
        return $ if isNothing arr then
            TypeNameDotClass typeName0
        else
            TypeNameArrDotClass typeName0

voidDotClass :: JParser Expression
voidDotClass =
        keyword "void" >> dot >> keyword "class" >>
        return VoidDotClass

this :: JParser Expression
this = keyword "this" >> return This
