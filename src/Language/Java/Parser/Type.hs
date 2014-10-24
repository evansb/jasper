{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Type where

import qualified Data.Set as S hiding (map)

import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)

import Text.Parsec.Combinator
import Text.Parsec.Prim

import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.AST

-- | Java types
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
refType = (try classOrInterfaceT <|> (ArrayType <$> arrayType))
       <?> "reference type"

classOrInterfaceT :: JParser RefType
classOrInterfaceT = ClassOrInterfaceType <$> (classType `sepBy1` dot)

classType :: JParser ClassType
classType = ClassType <$> ident <*> optionMaybe typeArgs
         <?> "class type"

arrayDims :: JParser ()
arrayDims = pure () <* many (lSquare <* rSquare)

arrayType :: JParser ArrayType
arrayType = (try (PrimArrayT <$> primType)
              <|> (RefArrayT <$> classOrInterfaceT))
         <* arrayDims

typeArgs :: JParser [TypeArg]
typeArgs = between lessThan greaterThan
        ((:) <$> typeArg <*> many (comma *> typeArg))
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
