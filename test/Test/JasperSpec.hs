module Test.JasperSpec where

import Text.Parsec
import Test.Hspec
import Test.Misc

import Language.Java.Parser.Internal
import Language.Java.Parser.Core
import Language.Java.Parser

testFromFile :: (Show a, Eq a, Read a) => JParser a -> String -> Expectation
testFromFile parser fileName = do
        content <- readFile fileName
        let pairs = read content :: (Read a) => [(String, a)]
        parser `shouldParseJ` pairs

spec :: Spec
spec = describe "Jasper Unit Test" $ do
        it "Should be able to parse expressions" $
            testFromFile expression "unit/expression"
        it "Should be able to parse statements" $
            testFromFile statement "unit/statement"
        it "Should be able to parse class declarations " $
            testFromFile classDeclaration "unit/class"
