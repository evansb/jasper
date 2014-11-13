
module Test.Language.Java.BasicParserSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST
import Language.Java.Lexer.Internal (javaReservedNames)
import Language.Java.Parser.Internal

spec :: Spec
spec = describe "Basic Parser" $ do
        it "Should be able to parse identifiers" $ do
            ident `shouldParseJ`
                [ "Boo" `to` Ident "Boo"
                , "foobar2000" `to` Ident "foobar2000"
                ]
            ident `shouldFailOnJ` javaReservedNames
        it "Should be able to parse names" $ do
            typeName `shouldParseJ`
                [ "foo.bar.baz" `to` TypeName (map Ident ["foo", "bar", "baz"])
                , "foo" `to` TypeName [Ident "foo"]
                ]
            typeName `shouldFailOnJ` [".Boo","Boo."]
