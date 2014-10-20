{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Type where

import qualified Data.Set as S hiding (map)

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Text.Parsec.Combinator
import Text.Parsec.Prim

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.AST

-- Java types
type_ :: JParser Type
type_ = try (PrimType <$> primType)
     <|> (RefType <$> refType)

-- | Primitive types
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

-- | Reference types
refType :: JParser RefType
refType = (try classOrInterfaceT <|> arrayType)
       <?> "reference type"

classOrInterfaceT :: JParser RefType
classOrInterfaceT = ClassOrInterfaceType <$> (classType `sepBy1` dot)

classType :: JParser ClassType
classType = (,) <$> ident <*> optionMaybe typeArgs
         <?> "class type"

arrayType :: JParser RefType
arrayType = ArrayType <$>
    (try (PrimArrayT <$> primType <* lSquare <* rSquare)
    <|> RefArrayT <$> classOrInterfaceT <* lSquare <* rSquare)

typeArgs :: JParser [TypeArg]
typeArgs = between lessThan greaterThan (do
    must <- typeArg
    opt <- many (comma *> typeArg)
    return (must : opt))
    <?> "type arguments"

typeArg :: JParser TypeArg
typeArg = do
    -- Check if there is an ?
    let wc x = isOperator x && (x === "?")
    tok <- try (getSS <$> satisfy wc) <|> return ""
    if null tok then
        ActualType <$> refType
    else
        Wildcard <$> optionMaybe wildcardBound
    ; <?> "type argument"

-- | Wildcard bounds
wildcardBound :: JParser WildcardBound
wildcardBound = do
    let good x = isKeyword x && ((x === "super") || (x === "extends"))
    tok <- satisfy good
    tRefType <- refType
    return $ if tok === "super"
                 then SuperWB tRefType
                 else ExtendsWB tRefType
    ; <?> "wildcard bound"

-- Misc functions
primitiveTypes = S.fromList
      [ "byte" , "short" , "int" , "long" , "char",
        "float" , "double" , "boolean"]