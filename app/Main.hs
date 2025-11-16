module Main where

import Lexer
import Parser

main :: IO ()
main = do 
    entry <- readFile "test.csym"
    let tokens = alexScanTokens entry
    -- print tokens
    let ast = unlines $ show <$> parseTokens tokens

    putStrLn ast
    writeFile "test.ast" ast
