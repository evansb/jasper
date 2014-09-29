
module Language.Java.Parser.Type where

import qualified Data.Set as S hiding (map)

import Control.Applicative ((<$>))

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Error

import Language.Java.Parser.Core
import Language.Java.AST (PrimType(..))

primitiveTypes = S.fromList
      [ "byte" , "short" , "int" , "long" , "char",
        "float" , "double" , "boolean"
      ]

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
