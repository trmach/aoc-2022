module Day7 where

import System.IO
import Parser

data File = File (String, Int) [File]

filename :: String
filename = "Day7-example.txt"

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print (run (many (some parseInstruction <|> parseDirContents)) contents)
    hClose handle