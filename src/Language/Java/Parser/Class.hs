
module Language.Java.Parser.Class where

import Control.Applicative ((*>), (<*>), (<$>))
import Text.Parsec.Combinator
import Text.Parsec.Prim

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Misc
import Language.Java.Parser.Basic
import Language.Java.Parser.Core
import Language.Java.Parser.Expression
import Language.Java.Parser.Type
import Language.Java.AST

superClass :: JParser SuperClass
superClass = keyword "extends" *> classType

superInterfaces :: JParser SuperInterfaces
superInterfaces = keyword "implements" *> (classType `sepBy1` comma)

classDeclaration :: JParser ClassDeclaration
classDeclaration = normalClassDeclaration

classModifiers :: JParser [ClassModifier]
classModifiers = many classModifier

-- TODO Add Annotation
classModifier :: JParser ClassModifier
classModifier = do
        tok <- getSS <$> getT
        case M.lookup tok classModifierTable of
            Just modifier -> return modifier
            Nothing -> unexpected "class modifier"

classBody :: JParser ClassBody
classBody = error "not implemented"

normalClassDeclaration :: JParser ClassDeclaration
normalClassDeclaration = Class
                      <$> classModifiers
                      <*> (keyword "class" *> ident)
                      <*> optionMaybe typeParams
                      <*> optionMaybe superClass
                      <*> optionMaybe superInterfaces
                      <*> classBody
                      <?> "normal class declaration"

variableDeclarator :: JParser VariableDeclarator
variableDeclarator = VariableDeclarator
                  <$> variableDeclaratorID
                  <*> optionMaybe (operator "=" *> variableInitializer)

variableDeclaratorID :: JParser VariableDeclID
variableDeclaratorID = (,) <$> ident <*> (fromMaybe 0 <$> optionMaybe arrayDims)
