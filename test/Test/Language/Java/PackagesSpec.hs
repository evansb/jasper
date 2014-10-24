
module Test.Language.Java.PackagesSpec where

import Test.Hspec
import Test.Misc

import Language.Java.AST
import Language.Java.Parser.Packages

spec :: Spec
spec = describe "Packages" $ do        
        it "Should be able to parse Single Type Import Declaration" $
            importDeclaration `shouldParseJ`
                [ "import foo.bar;" `to` SingleTypeImportDeclaration 
                                (TypeName [Ident "foo", Ident "bar"])
                ]
        it "Should be able to parse Type Import on Demand Declaration" $
            importDeclaration `shouldParseJ`
                [ "import foo.bar.*;" `to` TypeImportOnDemandDeclaration
                                (TypeName [Ident "foo", Ident "bar"])
                ]
        it "Should be able to parse Single Static Import Declaration" $
            importDeclaration `shouldParseJ`
                [ "import static foo.bar;" `to` SingleStaticImportDeclaration
                                (TypeName [Ident "foo"]) (Ident "bar")
                ]
            importDeclaration `shouldFailOnJ` [ "import static foo;"]
        it "Should be able to parse Static Import ond Demand Declaration" $
            importDeclaration `shouldParseJ`                
                [ "import static foo.bar.*;" `to` StaticImportOnDemandDeclaration
                                (TypeName [Ident "foo", Ident "bar"])
                ]
