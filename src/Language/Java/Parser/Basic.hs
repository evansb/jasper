
module Language.Java.Parser.Basic where

import Control.Applicative ((<$>))
import Text.Parsec.Combinator
import Language.Java.Parser.Core
import Language.Java.AST (Ident(..), TypeName(..))

ident :: JParser Ident
ident = Ident <$> (satisfy isIdentifier >>= getS)

typeName :: JParser TypeName
typeName = TypeName <$> ident `sepBy1` satisfy isPeriod
