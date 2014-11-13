
module Test.Language.Java.LexerSpec where

import Test.Hspec
import Test.Misc

import qualified Language.Java.AST as A
import qualified Language.Java.Lexer.Internal as L

import Language.Java.Lexer

isKeyword :: A.Token -> Bool
isKeyword (tok, _) = case tok of
                         A.Keyword _ -> True
                         _ -> False

countKeywords :: [A.Token] -> Int
countKeywords = length . filter id . map isKeyword

spec :: Spec
spec = describe "Lexer" $ do
        it "Should be able to parse string literals" $ do
            L.stringLiteral `shouldParse`
                [
                      "\"\""             `to`  ""
                    , "\"Hello World\""  `to` "Hello World"
                    , "\"\\n\""          `to` "\n"
                    , "\"Hello\\n\""     `to` "Hello\n"
                    , "\"\\120\""        `to` "P"
                ]
            L.stringLiteral `shouldFailOn`
                [
                      "\""                -- Missing "
                    , "\"\'"              -- Missing "
                    , "\"\\x\""           -- Unknown escape \x
                ]
        it "Should be able to parse reserved keywords" $ do
            let rn = L.javaReservedNames
            let r = unwords rn
            let tokens = runTokenizer r
            countKeywords tokens `shouldBe` length rn
        it "Should be able to parse integer literals" $ do
            L.integerLiteral `shouldParse`
                [
                      "1234"        `to`  1234
                    , "0b1010"      `to`  10
                    , "01123"       `to`   0o1123
                    , "0xABCDEF123" `to`  0xABCDEF123
                ]
            L.stringLiteral `shouldFailOn`
                [
                      "x202020"
                    , "0b333"
                    , "0oFFF"
                    , "0xx"
                ]
        it "Should be able to parse >> and << as two operators" $
            L.opLiteral `shouldParse` [ "<<" `to` "<", ">>" `to` ">"]
