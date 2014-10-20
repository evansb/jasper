module Test.Language.Java.ExpressionSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST

import Language.Java.Parser.Expression

spec :: Spec
spec = describe "Expression Parser" $ do
        it "Should be able to parse literal expression" $ do
            expression `shouldParseJ`
                [   "1230"      `to` Literal (IntegerLiteral 1230)
                  , "\"short\"" `to` Literal (StringLiteral "short")
                  , "20e3"      `to` Literal (FloatingPointLiteral "20e3")
                  , "'a'"    `to` Literal (CharacterLiteral 'a')]
            expression `shouldFailOnJ` [ "\"xxx", "'Hello'" ]
        it "Should be able to parse typename followed by dot class" $ do
            expression `shouldParseJ`
                [   "Mocha.Latte.class" `to` TypeNameDotClass
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])
                  , "Mocha.Latte[][][].class" `to` TypeNameArrDotClass 3
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])
                ]
            expression `shouldFailOnJ`
                [ "void.void", "Mocha.latte"]
        it "Should be able to parse typename followed by dot this" $ 
            expression `shouldParseJ`
                [ "Mocha.Latte.this" `to` TypeNameDotThis
                                        (TypeName [Ident "Mocha",
                                                   Ident "Latte"])]
        it "Should be able to parse void dot class" $
            expression `shouldParseJ` ["void.class" `to` VoidDotClass]
        it "Should be able to parse this" $
            expression `shouldParseJ`  ["this" `to` This]
