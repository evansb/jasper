
module Language.Java.Parser.Basic where

import Control.Applicative ((<$>))
import Text.Parsec.Combinator
import Language.Java.Parser.Core
import Language.Java.AST (Ident(..), TypeName(..))

-- | Java Identifier
ident :: JParser Ident
ident = Ident <$> (getSS <$> satisfy isIdentifier)

-- | Java Name
typeName :: JParser TypeName
typeName = TypeName <$> ident `sepBy1` dot

-- | Java Name Dot, for parsing Name dot class
typeNameDot :: JParser TypeName
typeNameDot = TypeName <$> ident `sepEndBy1` dot
