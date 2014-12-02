
module Main where

import Control.Monad
import System.IO
import System.Directory
import Criterion.Main

import Language.Java.Lexer
import Language.Java.Parser

headerStr :: String -> String -> String
headerStr file content = "File : " ++ file ++ " of " ++ show (length content)
                        ++ " characters."

notJava :: String -> Bool
notJava "." = True
notJava str = (length str - length ".java") < 0 ||
            (drop (length str - length ".java") str /= ".java")

benchFile :: String -> IO ()
benchFile file = unless (notJava file) $ do
        putStrLn ("Benchmarking " ++ file)
        content <- readFile ("./java-src/" ++ file)
        defaultMain [bgroup "lexer"  [bench file $ whnf lexJava content]]
        let tokens = lexJava content
        defaultMain [bgroup "parser" [bench file $ whnf parseTokens tokens]]

main :: IO ()
main = getDirectoryContents "java-src/" >>= mapM_ benchFile
