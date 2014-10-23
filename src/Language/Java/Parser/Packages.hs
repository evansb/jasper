{-# LANGUAGE  DoAndIfThenElse #-}
module Language.Java.Parser.Packages where

import Control.Applicative ((<$>),(*>),(<*), (<*>))
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Language.Java.Parser.Core
import Language.Java.Parser.Basic
import Language.Java.AST (
        CompilationUnit(..), 
        PackageDeclaration(..),
        PackageModifier(..),
        ImportDeclaration(..),
        TypeDeclaration(..))

-- | Compilation unit
compilationUnit :: JParser CompilationUnit
compilationUnit = CompilationUnit 
        <$> optionMaybe packageDeclaration
        <*> many importDeclaration
        <*> many typeDeclaration
        <?> "compilation unit"

-- | Package declaration
packageDeclaration :: JParser PackageDeclaration
packageDeclaration = PackageDeclaration
        <$> many packageModifier
        <*> typeName 
        <*  semiColon
        <?> "package declaration"        

-- | Package Modifier = Annotation
-- | TODO Change this once Annotation is finished
packageModifier :: JParser PackageModifier
packageModifier = return PackageModifier
               <?> "package modifier"

-- | Import declaration
importDeclaration :: JParser ImportDeclaration
importDeclaration = choice (map try [
          singleTypeImportDeclaration
        , typeImportOnDemandDeclaration
        , singleStaticImportDeclaration
        , staticImportOnDemandDeclaration
        ])
        <?> "import declaration"

singleTypeImportDeclaration :: JParser ImportDeclaration
singleTypeImportDeclaration = SingleTypeImportDeclaration
        <$> (keyword "import" *> typeName)
        <* semiColon

typeImportOnDemandDeclaration :: JParser ImportDeclaration
typeImportOnDemandDeclaration = TypeImportOnDemandDeclaration
        <$> (keyword "import" *> typeName <* dot <* star <* semiColon)

singleStaticImportDeclaration :: JParser ImportDeclaration
singleStaticImportDeclaration = SingleStaticImportDeclaration 
        <$> (keyword "import" *> keyword "static" *> typeName)
        <*> (dot *> ident)
 
staticImportOnDemandDeclaration :: JParser ImportDeclaration 
staticImportOnDemandDeclaration = StaticImportOnDemandDeclaration
        <$> ((keyword "import" *> keyword "static" *> typeName <*
            (dot <* star <* semiColon)))

typeDeclaration :: JParser TypeDeclaration
typeDeclaration = undefined
