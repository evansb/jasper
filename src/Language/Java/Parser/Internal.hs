
module Language.Java.Parser.Internal where

import Text.Parsec hiding (satisfy)
import qualified Language.Java.AST as A

type JParser a = Parsec [A.Token] () a

getKeyword (A.Keyword k) = k
getKeyword other = show other

nextPos :: SourcePos -> t -> [A.Token] -> SourcePos
nextPos _ _ ((_, pos) : _) = pos
nextPos pos _ [] = pos

satisfy :: (A.Token -> Bool) -> JParser A.T
satisfy f = tokenPrim show nextPos
                (\c -> if f c then Just (fst c) else Nothing)

getT :: JParser A.T
getT = tokenPrim show nextPos (Just . fst)
