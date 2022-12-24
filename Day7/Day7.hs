module Day7 where

import System.IO
import Parser

--data Tree a = Node a [Tree a] deriving Show

filename :: String
filename = "Day7-example.txt"

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print contents
    hClose handle