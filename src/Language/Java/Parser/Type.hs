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
