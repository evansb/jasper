
module Test.Language.Java.TypeParserSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST (PrimType(..))

import Language.Java.Parser.Internal

spec :: Spec
spec = describe "Types Parser" $
        it "Should be able to parse primitive types" $ do
            primType `shouldParseJ`
                [   "byte"    `to` ByteT
                  , "short"   `to` ShortT
                  , "int"     `to` IntT
                  , "long"    `to` LongT
                  , "char"    `to` CharT
                  , "float"   `to` FloatT
                  , "double"  `to` DoubleT
                  , "boolean" `to` BooleanT ]
            primType `shouldFailOnJ` [ "Boolean", "Integer", "foo" ]

