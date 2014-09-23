
module Language.Java.Parser.Core where

import Text.Parsec hiding (satisfy)
import qualified Language.Java.AST as A

type JParser a = Parsec [A.Token] () a

-- | Advance token position
nextPos :: SourcePos -> t -> [A.Token] -> SourcePos
nextPos _ _ ((_, pos) : _) = pos
nextPos pos _ [] = pos;

-- | Same as satisfy, this is for token
satisfy :: (A.T -> Bool) -> JParser A.T
satisfy f = tokenPrim show nextPos
                (\c -> if f (fst c) then Just (fst c) else Nothing)

-- | Fetch next token and advance
getT :: JParser A.T
getT = tokenPrim show nextPos (Just . fst)

getS :: A.T -> JParser String
getS (A.TokIdent   s) = return s
getS (A.Keyword    s) = return s
getS (A.Operator   s) = return s
getS _ = undefined

-- Misc. functions --
isIdentifier (A.TokIdent _) = True
isIdentifier _ = False

isPeriod A.Period = True
isPeriod _ = False
