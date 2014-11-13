module Test.Language.Java.ExpressionSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST
import Language.Java.Parser.Internal

spec :: Spec
spec = describe "Primary Expression Parser" $ do
        it "Should be able to parse literal expression" $ do
            primary `shouldParseJ`
                [   "1230"      `to` Literal (IntegerLiteral 1230)
                  , "\"short\"" `to` Literal (StringLiteral "short")
                  , "20e3"      `to` Literal (FloatingPointLiteral "20e3")
                  , "'a'"       `to` Literal (CharacterLiteral 'a')]
            primary `shouldFailOnJ` [ "\"xxx", "'Hello'" ]
        it "Should be able to parse typename followed by dot class" $ do
            primary `shouldParseJ`
                [   "Mocha.Latte.class" `to` TypeNameDotClass
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])
                  , "Mocha.Latte[][][].class" `to` TypeNameArrDotClass
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])
                                        3
                ]
            primary `shouldFailOnJ`
                [ "void.void", "Mocha.latte"]
        it "Should be able to parse typename followed by dot this" $
            primary `shouldParseJ`
                [ "Mocha.Latte.this" `to` TypeNameDotThis
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])]
        it "Should be able to parse void dot class" $
            primary `shouldParseJ` ["void.class" `to` VoidDotClass]
        it "Should be able to parse this" $
            primary `shouldParseJ`  ["this" `to` This]
