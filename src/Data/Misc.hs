
module Data.Misc where

import qualified Data.HashSet as S hiding (map)
import qualified Data.Map as M
import qualified Language.Java.AST as A

reservedNames = [
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char",
    "class", "const", "continue", "default", "do", "double", "else", "enum",
    "extends", "final", "finally", "float", "for", "goto", "if", "implements",
    "import", "instanceof", "int", "interface", "long", "native", "new",
    "package", "private", "protected", "public", "return", "short", "static",
    "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
    "transient", "try", "void", "volatile", "while"]

reservedOpNames = [
    "!=", "!",
    ">>>=", ">>>", ">>=", ">=", ">>", ">",
    "==", "=",
    "|", "|=", "||",
    "&&", "&=", "&",
    "^=", "^",
    "%", "%=",
    "*=", "*",
    "++", "+=", "+",
    "--", "-=", "-",
    "/=", "/",
    ":",
    "<<=", "<<", "<=", "<",
    "?"
    ]

classModifierTable :: M.Map String A.ClassModifier
classModifierTable = M.fromList
                   [ ("public"     , A.Public)
                   , ("protected"  , A.Protected)
                   , ("private"    , A.Private)
                   , ("abstract"   , A.Abstract)
                   , ("static"     , A.Static)
                   , ("final"      , A.Final)
                   , ("strictfp"   , A.StrictFP)
                   ]

keywordTable = S.fromList reservedNames
operatorTable = S.fromList reservedOpNames
isKeyword = flip S.member keywordTable
isOperator = flip S.member operatorTable
