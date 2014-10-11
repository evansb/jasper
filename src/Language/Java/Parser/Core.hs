
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

getSS :: A.T -> String
getSS (A.TokIdent   s) = s
getSS (A.Keyword    s) = s
getSS (A.Operator   s) = s
getSS _ = undefined

(===) :: A.T -> String -> Bool
t === s = getSS t == s

-- Misc. functions --
isIdentifier (A.TokIdent _) = True
isIdentifier _ = False

isKeyword (A.Keyword _) = True
isKeyword _ = False

isPeriod A.Period = True
isPeriod _ = False

isOperator (A.Operator _) = True
isOperator  _ = False

isComma A.Comma = True
isComma _ = False

isLSquare A.LSquare = True
isLSquare _ = False

isRSquare A.RSquare = True
isRSquare _ = False

isLParen A.LParen = True
isLParen _ = False

isRParen A.RParen = True
isRParen _ = False

lessThan = satisfy (\x -> isOperator x && x === "<")
greaterThan = satisfy (\x -> isOperator x && x === ">")
comma = satisfy isComma
lSquare = satisfy isLSquare
rSquare = satisfy isRSquare
lParen = satisfy isLParen
rParen = satisfy isRParen
dot = satisfy isPeriod
keyword kwd = satisfy (\x -> isKeyword x && (getSS x == kwd))
