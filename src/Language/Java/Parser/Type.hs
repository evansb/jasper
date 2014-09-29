{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Type where

import qualified Data.Set as S hiding (map)

import Control.Applicative ((<$>))

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Error

import Language.Java.Parser.Core
import Language.Java.AST (PrimType(..), RefType(..), WildcardBound(..))

primitiveTypes = S.fromList
      [ "byte" , "short" , "int" , "long" , "char",
        "float" , "double" , "boolean"
      ]

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
    <?> "primitive types"

-- | Reference types
refType :: JParser RefType
refType = undefined

-- | Wildcard bounds
wildcardBound :: JParser WildcardBound
wildcardBound = do
    let good x = isKeyword x && ((x === "super") || (x === "extends"))
    tok <- satisfy good
    tRefType <- refType
    return $ if tok === "super"
                 then SuperWB tRefType
                 else ExtendsWB tRefType
