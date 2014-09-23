
module Language.Java.Parser.Basic where

import Control.Applicative ((<$>))
import Text.Parsec.Combinator
import Language.Java.Parser.Core
import Language.Java.AST (Ident(..), Name(..))

ident :: JParser Ident
ident = Ident <$> (satisfy isIdentifier >>= getS)

name :: JParser Name
name = Name <$> ident `sepBy1` satisfy isPeriod
