
module Main where

import Control.Monad
import System.IO
import System.Directory
import Criterion.Main
import Language.Java.Lexer
import Language.Java.AST

runLexer :: String -> IO String
runLexer str = return (show (lexJava str))

headerStr :: String -> String -> String
headerStr file content = "File : "
                        ++ file ++ " of "
                        ++ show (length content)
                        ++ " characters."

notJava :: String -> Bool
notJava "." = True
notJava str = (length str - length ".java") < 0 ||
            (drop (length str - length ".java") str /= ".java")


benchFile :: String -> IO ()
benchFile file = unless (notJava file) $ do
        putStrLn ("Benchmarking " ++ file)
        content <- readFile ("./java-src/" ++ file)
        defaultMain [bench (headerStr file content) $ nfIO (runLexer content)]

main :: IO ()
main = getDirectoryContents "java-src/" >>= mapM_ benchFile
