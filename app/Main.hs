module Main where
import System.Environment

import Lexer
import Parser
import Printer  

parseInput :: String -> IO ()
parseInput input = do 
    let tokens       = alexScanTokens input 
        ast          = parseTokens tokens
        outputString = unlines $ pshow <$> ast
    putStrLn outputString
    writeFile "ast.ast" outputString

main :: IO ()
main = do
    args <- getArgs
    case args of 
      [file] -> do
        entry <- readFile file
        parseInput entry
      _ -> putStrLn "Wrong number of arguments \nUse \"cabal run . -- filename\" to parse a file"
