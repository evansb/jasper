

module Test.Language.Java.BasicParserSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST
import Data.Misc (reservedNames)

import Language.Java.Parser.Basic

spec :: Spec
spec = describe "Basic Parser" $ do
        it "Should be able to parse identifiers" $ do
            ident `shouldParseJ`
                [ "Boo" `to` Ident "Boo"
                , "foobar2000" `to` Ident "foobar2000"
                ]
            ident `shouldFailOnJ` reservedNames
        it "Should be able to parse names" $ do
            let test1 = "foo.bar.baz"
            let exp1 = TypeName (map Ident ["foo", "bar", "baz"])
            typeName `shouldParseJ` [test1 `to` exp1]
            typeName `shouldFailOnJ` [".Boo","Boo."]