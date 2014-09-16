
module Test.Misc where

import Test.Hspec

import Text.Parsec.String
import Text.Parsec.Prim

shouldParse :: (Show a, Eq a) => Parser a -> [(String, a)] -> Expectation
shouldParse parser =
        mapM_ (\(source, expected) ->
            case parse parser "" source of
                Left  _      -> expectationFailure ("Test case \"" ++
                                    source ++ "\" failed")
                Right result -> result `shouldBe` expected)

shouldFailOn :: (Show a, Eq a) => Parser a -> [String] -> Expectation
shouldFailOn parser =
        mapM_ (\source ->
            case parse parser "" source of
                Left  _ -> True `shouldBe` True
                Right _ -> expectationFailure ("Test case \"" ++ source ++ "\" failed"))
to :: a -> b -> (a, b)
to = (,)
