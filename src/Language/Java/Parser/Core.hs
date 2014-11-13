
module Language.Java.Parser.Core where

import Text.Parsec hiding (satisfy)
import qualified Data.Map as M

import Language.Java.AST

-- | Type of the Java Parser
type JParser a = Parsec [Token] () a

-- | Advance token position
nextPos :: SourcePos -> t -> [Token] -> SourcePos
nextPos _ _ ((_, pos) : _) = pos
nextPos pos _ [] = pos

-- | Same as Parsec's satisfy, this is for Java token
satisfy :: (T -> Bool) -> JParser T
satisfy f = tokenPrim show nextPos
                (\c -> if f (fst c) then Just (fst c) else Nothing)

-- | Fetch next token and advance
getT :: JParser T
getT = tokenPrim show nextPos (Just . fst)

-- | Get the string from a Token context
-- | Will fail if the Token does not store a String
getSS :: T -> String
getSS (TokIdent   s) = s
getSS (Keyword    s) = s
getSS (Operator   s) = s
getSS _                = error "Non string storing token"

(===) :: T -> String -> Bool
t === s = getSS t == s

isIdentifier (TokIdent _) = True
isIdentifier _ = False

isKeyword (Keyword _) = True
isKeyword _ = False

isPeriod Period = True
isPeriod _ = False

isOperator (Operator _) = True
isOperator  _ = False

isComma Comma = True
isComma _ = False

isLSquare LSquare = True
isLSquare _ = False

isRSquare RSquare = True
isRSquare _ = False

isLParen LParen = True
isLParen _ = False

isRParen RParen = True
isRParen _ = False

isSemiColon SemiColon = True
isSemiColon _ = False

isDColon DColon = True
isDColon _ = False

isLBrace LBrace = True
isLBrace _ = False

isRBrace RBrace = True
isRBrace _ = False

operator s      = satisfy (\x -> isOperator x && x === s)
keyword kwd     = satisfy (\x -> isKeyword x  && x === kwd)

lessThan        = operator "<"      >> return OpLT
greaterThan     = operator ">"      >> return OpGT
star            = operator "*"      >> return OpMult
multOp          = star

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

-- | Stores mapping between keyword and class modifier
classModifierTable :: M.Map String ClassModifier
classModifierTable = M.fromList [
    ("public", Public), ("protected", Protected), ("private", Private),
    ("static", Static), ("final", Final), ("strictfp", StrictFP),
    ("abstract", Abstract)]

-- | Stores mapping between keyword and field modifier
fieldModifierTable :: M.Map String FieldModifier
fieldModifierTable = M.fromList [
    ("public", PublicF), ("protected", ProtectedF), ("private", PrivateF),
    ("static", StaticF), ("final", FinalF), ("transient", TransientF),
    ("volatile", VolatileF)]

-- | Stores mapping between keyword and method modifier
methodModifierTable :: M.Map String MethodModifier
methodModifierTable = M.fromList [
    ("public", PublicM), ("protected", ProtectedM), ("private", PrivateM),
    ("static", StaticM), ("final", FinalM), ("transient", TransientM),
    ("synchronized", SynchronizedM), ("native", NativeM),
    ("strictfp", StrictFPM)]
