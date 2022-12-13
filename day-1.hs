import MP4
import System.IO
import Control.Monad
import Data.List.Split
import Data.List

filename = "day-1.txt"

f :: [[Int]] -> [[String]] -> [[Int]] -- hell
f _ [] = []
f c (x:xs) = (map (read :: String -> Int) x) : (f c xs)


main :: IO ()
main = do
    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print $ sum $ take 3 $ (reverse . sort) (map sum (f [[]] (splitWhen (=="") (lines contents))))
    hClose handle