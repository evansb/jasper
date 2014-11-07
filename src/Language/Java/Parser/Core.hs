
module Language.Java.Parser.Core where

import Text.Parsec hiding (satisfy)
import qualified Language.Java.AST as A

-- | Type of the Java Parser
type JParser a = Parsec [A.Token] () a

-- | Advance token position
nextPos :: SourcePos -> t -> [A.Token] -> SourcePos
nextPos _ _ ((_, pos) : _) = pos
nextPos pos _ [] = pos;

-- | Same as Parsec's satisfy, this is for Java token
satisfy :: (A.T -> Bool) -> JParser A.T
satisfy f = tokenPrim show nextPos
                (\c -> if f (fst c) then Just (fst c) else Nothing)

-- | Fetch next token and advance
getT :: JParser A.T
getT = tokenPrim show nextPos (Just . fst)

-- | Get the string from a Token context
-- | Will fail if the Token does not store a String
getSS :: A.T -> String
getSS (A.TokIdent   s) = s
getSS (A.Keyword    s) = s
getSS (A.Operator   s) = s
getSS _ = undefined

(===) :: A.T -> String -> Bool
t === s = getSS t == s

-- | Miscellaneous functions
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

isSemiColon A.SemiColon = True
isSemiColon _ = False

isDColon A.DColon = True
isDColon _ = False

isLBrace A.LBrace = True
isLBrace _ = False

isRBrace A.RBrace = True
isRBrace _ = False

operator s = satisfy (\x -> isOperator x && x === s)

lessThan        = operator "<"
greaterThan     = operator ">"
lessThanEq      = operator "<="
greaterThanEq   = operator ">="
star            = operator "*"

multOp          = operator "*"
addOp           = operator "+"
incOp           = operator "++"
minOp           = operator "-"
decOp           = operator "--"

comma           = satisfy isComma
lSquare         = satisfy isLSquare
rSquare         = satisfy isRSquare
lParen          = satisfy isLParen
rParen          = satisfy isRParen
lBrace          = satisfy isLBrace
rBrace          = satisfy isRBrace
semiColon       = satisfy isSemiColon
dot             = satisfy isPeriod
dColon          = satisfy isDColon

keyword kwd     = satisfy (\x -> isKeyword x && (getSS x == kwd))
