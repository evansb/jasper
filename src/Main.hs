
module Main where

import Language.Java.Lexer
import Language.Java.Parser
import System.Environment
import System.IO

main :: IO ()
main = getArgs >>= lexAndParse

lexAndParse :: [String] -> IO ()
lexAndParse [fs] = do
        content <- readFile fs
        let tokens = lexJava content
        writeFile (fs ++ ".tok") (show tokens)
        let parsed = parseTokens tokens
        writeFile (fs ++ ".pt") (show parsed)

lexAndParse ["lexonly", fs] = do
        content <- readFile fs
        let tokens = lexJava content
        writeFile (fs ++ ".tok") (show tokens)

lexAndParse _ = putStrLn "Usage : jasper <file.java>"
