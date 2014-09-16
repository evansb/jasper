
module Language.Java.Parser.Internal where

import Text.Parsec hiding (satisfy)
import qualified Language.Java.AST as A
import qualified Data.Map as M

type JParser a = Parsec [A.Token] () a

integralTypes = M.fromList
    [ ("byte", A.JByte), ("long", A.JLong),
        ("short", A.JShort), ("int", A.JInt), ("char", A.JChar) ]

floatTypes = M.fromList [ ("float", A.JFloat), ("double", A.JDouble)]

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

integralType :: JParser A.T
integralType = satisfy (\t -> case t of
                    (A.Keyword k, _) -> M.member k integralTypes
                    (_,_)           -> False)

primitiveType :: JParser A.PrimitiveType
primitiveType = do
     tok <- getT
     let e = getKeyword tok
     let ityp = M.lookup e integralTypes
     let ftyp = M.lookup e floatTypes
     let btyp = e == "boolean"
     case (ityp, ftyp, btyp) of
         (Just i, Nothing, False) -> return $ A.NumericType i
         (Nothing, Just f, False) -> return $ A.NumericType f
         (Nothing, Nothing, True) -> return A.BooleanType
         _ -> unexpected e
    <?> "primitive type"
