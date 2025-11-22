module Main where

import Lexer
import Parser

main :: IO ()
main = do 
    entry <- readFile "test.csym"
    let tokens = alexScanTokens entry
    let ast = unlines $ pretty <$> parseTokens tokens

    putStrLn ast
    writeFile "test.ast" ast
