module Main where
import System.Environment

import Lexer
import Parser
import Printer

main :: IO ()
main = do
    args <- getArgs
    case args of 
      [file] -> do
        entry <- readFile file
        let tokens = alexScanTokens entry
        let ast = unlines $ innerStr. (pretty 0) <$> parseTokens tokens

        putStrLn ast
        writeFile "test.ast" ast
      _ -> putStrLn "Wrong number of arguments"
